(in-package #:canvas-protocol)

;;; Inputs and forms. NAME matches [A-Za-z0-9_.-]+ (else CANVAS-ERROR).
;;; Every input is wrapped in <label> (label text first, input after;
;;; checkbox: input first, then the text). Input attribute order:
;;; type name value <kind-specific> required aria-invalid aria-describedby.
;;;
;;; text-input   <label>L<input type="text" name="N" [value] [placeholder] [required=""]></label>
;;; number-input <input type="number" name [value] [min] [max] [step] [required]>
;;; date-input   <input type="date" name [value] [min] [max] [required]>; a value
;;;              (y m d) list prints YYYY-MM-DD; a string passes as is
;;; checkbox     <label><input type="checkbox" name value="VALUE" [checked=""]>L</label>; value default "on"
;;; select-input <label>L<select name="N" [required]><option value="v" [selected=""]>label</option>...</select></label>;
;;;              OPTIONS strings or (value . label)
;;; textarea     <label>L<textarea name="N" [rows] [placeholder] [required]>value</textarea></label>
;;; submit-button <button type="submit" [name] [value]>label</button>
;;; form         <form [id] hx-post="URL" [hx-target] [hx-swap]>children</form>;
;;;              with ID and no TARGET: hx-target="#ID" hx-swap="outerHTML";
;;;              explicit TARGET / SWAP win; no ID and no TARGET: hx-post only.
;;;              LINK resolving to NIL: no hx attributes.
;;; ERROR (a CANVAS-COERCION-ERROR) on an input: aria-invalid="true"
;;; aria-describedby="<field>-error" on the control and ERROR-FRAGMENT as the
;;; label's last child. ERROR-FRAGMENT:
;;;   <small id="<field>-error" role="alert">COERCION-ERROR-TEXT</small>
;;;
;;; COERCE-PARAM value type &key field default required min max choices
;;; - VALUE a string or NIL. NIL or "" (after trimming spaces/tabs) is missing:
;;;   REQUIRED -> error :required, else DEFAULT.
;;; - :string the string (untrimmed); :integer [+-]?[0-9]+ (trimmed);
;;;   :float [+-]?(digits[.digits*]|.digits)([eE][+-]?digits)? -> DOUBLE-FLOAT;
;;;   :boolean on/true/1/yes -> T, off/false/0/no -> NIL (case-insensitive);
;;;   :date YYYY-MM-DD, a real Gregorian date -> (y m d);
;;;   :choice member of CHOICES (strings or (value . label); compared with
;;;   STRING=) -> the value string; detail = the list of values.
;;;   MIN / MAX (numbers) check :integer and :float results -> :min / :max.
;;;   Never the Lisp reader. Unknown TYPE -> CANVAS-ERROR (not a coercion error).
;;; - Failure: CANVAS-COERCION-ERROR (field value expected detail) signalled
;;;   with ERROR inside restarts USE-VALUE (v) -> v and USE-DEFAULT () -> DEFAULT.
;;; COERCE-FORM params specs -> (values plist errors). SPECS: (key type . keys)
;;; where keys are COERCE-PARAM's (:field defaults to the lowercased key name).
;;; The plist holds every spec key in spec order; each coercion error is
;;; collected (in spec order) and USE-DEFAULT taken. FIND-FIELD-ERROR errors
;;; field -> the error for FIELD or NIL.

(defclass input-component (component)
  ((name :initarg :name :reader input-name)
   (label :initarg :label :initform nil :reader input-label)
   (value :initarg :value :initform nil :reader input-value)
   (required :initarg :required :initform nil :reader input-required-p)
   (error :initarg :error :initform nil :reader input-error)))

(defclass text-input (input-component)
  ((placeholder :initarg :placeholder :initform nil :reader input-placeholder)))

(defun text-input (name &key label value placeholder required error id attrs)
  (%make 'text-input :name name :label label :value value :placeholder placeholder
                     :required required :error error :id id :attrs attrs))

(defclass number-input (input-component)
  ((min :initarg :min :initform nil :reader input-min)
   (max :initarg :max :initform nil :reader input-max)
   (step :initarg :step :initform nil :reader input-step)))

(defun number-input (name &key label value min max step required error id attrs)
  (%make 'number-input :name name :label label :value value :min min :max max :step step
                       :required required :error error :id id :attrs attrs))

(defclass date-input (input-component)
  ((min :initarg :min :initform nil :reader input-min)
   (max :initarg :max :initform nil :reader input-max)))

(defun date-input (name &key label value min max required error id attrs)
  (%make 'date-input :name name :label label :value value :min min :max max
                     :required required :error error :id id :attrs attrs))

(defclass checkbox (input-component)
  ((checked :initarg :checked :initform nil :reader input-checked-p)))

(defun checkbox (name &key label checked (value "on") error id attrs)
  (%make 'checkbox :name name :label label :checked checked :value value
                   :error error :id id :attrs attrs))

(defclass select-input (input-component)
  ((options :initarg :options :reader input-options)))

(defun select-input (name options &key label value required error id attrs)
  (%make 'select-input :name name :options options :label label :value value
                       :required required :error error :id id :attrs attrs))

(defclass textarea (input-component)
  ((rows :initarg :rows :initform nil :reader input-rows)
   (placeholder :initarg :placeholder :initform nil :reader input-placeholder)))

(defun textarea (name &key label value rows placeholder required error id attrs)
  (%make 'textarea :name name :label label :value value :rows rows :placeholder placeholder
                   :required required :error error :id id :attrs attrs))

(defclass submit-button (component)
  ((label :initarg :label :reader submit-label)
   (name :initarg :name :initform nil :reader submit-name)
   (value :initarg :value :initform nil :reader submit-value)))

(defun submit-button (label &key name value id attrs)
  (%make 'submit-button :label label :name name :value value :id id :attrs attrs))

(defclass form (container-component)
  ((link :initarg :link :initform nil :reader form-link)
   (target :initarg :target :initform nil :reader form-target)
   (swap :initarg :swap :initform nil :reader form-swap)))

(defun form (children &key link id target swap attrs)
  (%make 'form :children children :link link :id id :target target :swap swap :attrs attrs))

;;; Validation

(defun %field-name-p (name)
  (flet ((ok (c) (or (%ascii-alnum-p c) (find c "_.-"))))
    (%token-p name #'ok #'ok)))

(defun %check-field-name (name)
  (unless (%field-name-p name)
    (%canvas-error "field name must match [A-Za-z0-9_.-]+, not ~S" name)))

(defun %option-value (option) (if (consp option) (car option) option))
(defun %option-label (option) (if (consp option) (cdr option) option))

(defmethod validate-component ((c input-component))
  (%check-field-name (input-name c))
  (let ((err (input-error c)))
    (unless (or (null err) (typep err 'canvas-coercion-error))
      (%canvas-error "input :error must be a canvas-coercion-error: ~S" err)))
  (call-next-method))

(defmethod validate-component ((c select-input))
  (let ((options (input-options c)))
    (unless (and (%proper-list-p options)
                 (every (lambda (o) (stringp (%option-value o))) options))
      (%canvas-error "select options must be strings or (value . label) conses: ~S" options)))
  (call-next-method))

(defmethod validate-component ((c submit-button))
  (when (submit-name c) (%check-field-name (submit-name c)))
  (call-next-method))

(defmethod validate-component ((c form))
  (let ((link (form-link c)))
    (unless (or (null link) (canvas-link-p link))
      (%canvas-error "form :link must be a canvas-link: ~S" link))
    (unless (or (null (form-target c)) (stringp (form-target c)))
      (%canvas-error "form :target must be a string: ~S" (form-target c)))
    (when (form-swap c) (hx-swap-value (form-swap c))))
  (call-next-method))

;;; Rendering

(defun %number-attr (value)
  "A number for an attribute: integers as is, other reals in plain decimal notation."
  (typecase value
    (integer value)
    (real (let ((*read-default-float-format* 'double-float))
            (format nil "~F" (coerce value 'double-float))))
    (t value)))

(defun %date-attr (value)
  (if (and (consp value) (= 3 (length value)) (every #'integerp value))
      (format nil "~4,'0D-~2,'0D-~2,'0D" (first value) (second value) (third value))
      value))

(defun %control-attrs (c type &rest kind)
  "Input attributes: id, type, name, value, KIND, required, aria-*, then the caller's attrs."
  (let ((err (input-error c)))
    (append (list :id (component-id c))
            (when type (list :type type))
            (list :name (input-name c))
            kind
            (list :required (and (input-required-p c) t))
            (when err
              (list :aria-invalid "true"
                    :aria-describedby (%str (coercion-error-field err) "-error")))
            (component-attrs c))))

(defun %labelled (c control &key control-first)
  (let ((err (and (input-error c) (error-fragment (input-error c)))))
    (if control-first
        (el :label nil control (input-label c) err)
        (el :label nil (input-label c) control err))))

(defmethod render ((c text-input))
  (%labelled c (el :input (%control-attrs c "text" :value (input-value c)
                                          :placeholder (input-placeholder c)))))

(defmethod render ((c number-input))
  (%labelled c (el :input (%control-attrs c "number" :value (%number-attr (input-value c))
                                          :min (%number-attr (input-min c))
                                          :max (%number-attr (input-max c))
                                          :step (%number-attr (input-step c))))))

(defmethod render ((c date-input))
  (%labelled c (el :input (%control-attrs c "date" :value (%date-attr (input-value c))
                                          :min (%date-attr (input-min c))
                                          :max (%date-attr (input-max c))))))

(defmethod render ((c checkbox))
  (%labelled c (el :input (%control-attrs c "checkbox" :value (input-value c)
                                          :checked (and (input-checked-p c) t)))
             :control-first t))

(defmethod render ((c select-input))
  (let ((value (input-value c)))
    (%labelled c (el :select (%control-attrs c nil)
                     (loop for option in (input-options c)
                           for v = (%option-value option)
                           collect (el :option (list :value v
                                                     :selected (and (stringp value)
                                                                    (string= v value)))
                                       (%option-label option)))))))

(defmethod render ((c textarea))
  (%labelled c (el :textarea (%control-attrs c nil :rows (input-rows c)
                                             :placeholder (input-placeholder c))
                   (input-value c))))

(defmethod render ((c submit-button))
  (el :button (%own c :type "submit" :name (submit-name c) :value (submit-value c))
      (submit-label c)))

(defmethod render ((c form))
  (let* ((id (component-id c))
         (target (form-target c))
         (link (form-link c))
         (hx (when link
               (hx-post link :target (or target (and id (%str "#" id)))
                             :swap (or (form-swap c) (and id (null target) :outer-html))))))
    (el :form (append (list :id id) hx (component-attrs c))
        (component-children c))))

(defmethod render ((c canvas-coercion-error))
  (error-fragment c))

(defun error-fragment (condition)
  "<small id=\"<field>-error\" role=\"alert\">text</small> for a CANVAS-COERCION-ERROR."
  (unless (typep condition 'canvas-coercion-error)
    (%canvas-error "error-fragment needs a canvas-coercion-error: ~S" condition))
  (el :small (list :id (%str (coercion-error-field condition) "-error") :role "alert")
      (coercion-error-text condition)))

;;; Coercion (never the Lisp reader)

(defun %digits-p (string &key (start 0) (end (length string)))
  (and (< start end)
       (loop for i from start below end always (%ascii-digit-p (char string i)))))

(defun %parse-integer-text (s)
  "[+-]?[0-9]+ -> integer, or NIL."
  (let ((start (if (and (plusp (length s)) (find (char s 0) "+-")) 1 0)))
    (when (%digits-p s :start start)
      (parse-integer s))))

(defun %parse-float-text (s)
  "[+-]?(digits[.digits*]|.digits)([eE][+-]?digits)? -> DOUBLE-FLOAT, or NIL."
  (let ((n (length s)) (i 0) (sign 1))
    (flet ((digits ()
             (let ((start i))
               (loop while (and (< i n) (%ascii-digit-p (char s i))) do (incf i))
               (values start i))))
      (when (and (< i n) (find (char s i) "+-"))
        (when (char= (char s i) #\-) (setf sign -1))
        (incf i))
      (multiple-value-bind (int-start int-end) (digits)
        (multiple-value-bind (frac-start frac-end)
            (if (and (< i n) (char= (char s i) #\.))
                (progn (incf i) (digits))
                (values i i))
          (when (and (= int-start int-end) (= frac-start frac-end))
            (return-from %parse-float-text nil))
          (let ((exponent 0))
            (when (and (< i n) (char-equal (char s i) #\e))
              (incf i)
              (let ((esign 1))
                (when (and (< i n) (find (char s i) "+-"))
                  (when (char= (char s i) #\-) (setf esign -1))
                  (incf i))
                (multiple-value-bind (e-start e-end) (digits)
                  (when (= e-start e-end) (return-from %parse-float-text nil))
                  (setf exponent (* esign (parse-integer s :start e-start :end e-end))))))
            (unless (= i n) (return-from %parse-float-text nil))
            (let* ((int (if (= int-start int-end) 0 (parse-integer s :start int-start :end int-end)))
                   (flen (- frac-end frac-start))
                   (frac (if (zerop flen) 0 (parse-integer s :start frac-start :end frac-end)))
                   (mantissa (+ int (/ frac (expt 10 flen)))))
              (cond ((zerop mantissa) (if (minusp sign) -0d0 0d0))
                    ((> (abs exponent) 400) nil)
                    (t (let ((r (* sign mantissa (expt 10 exponent))))
                         (when (<= least-positive-normalized-double-float (abs r)
                                   most-positive-double-float)
                           (coerce r 'double-float))))))))))))

(defun %leap-year-p (year)
  (or (and (zerop (mod year 4)) (plusp (mod year 100)))
      (zerop (mod year 400))))

(defun %parse-date-text (s)
  "YYYY-MM-DD, a real Gregorian date -> (y m d), or NIL."
  (when (and (= (length s) 10)
             (char= (char s 4) #\-) (char= (char s 7) #\-)
             (%digits-p s :start 0 :end 4) (%digits-p s :start 5 :end 7)
             (%digits-p s :start 8 :end 10))
    (let ((y (parse-integer s :start 0 :end 4))
          (m (parse-integer s :start 5 :end 7))
          (d (parse-integer s :start 8 :end 10)))
      (when (and (<= 1 m 12)
                 (<= 1 d (if (and (= m 2) (%leap-year-p y))
                             29
                             (nth (1- m) '(31 28 31 30 31 30 31 31 30 31 30 31)))))
        (list y m d)))))

(defun %coerce-typed (type value trimmed choices)
  "-> (values result ok)."
  (ecase type
    (:string (values value t))
    (:integer (let ((r (%parse-integer-text trimmed))) (values r (and r t))))
    (:float (let ((r (%parse-float-text trimmed))) (values r (and r t))))
    (:boolean
     (cond ((member trimmed '("on" "true" "1" "yes") :test #'string-equal) (values t t))
           ((member trimmed '("off" "false" "0" "no") :test #'string-equal) (values nil t))
           (t (values nil nil))))
    (:date (let ((r (%parse-date-text trimmed))) (values r (and r t))))
    (:choice
     (let ((v (find trimmed (mapcar #'%option-value choices) :test #'string=)))
       (values v (and v t))))))

(defun coerce-param (value type &key (field "value") default required min max choices)
  "Coerce the request string VALUE to TYPE. See the file header."
  (unless (member type +coercion-types+)
    (%canvas-error "unknown coercion type ~S (one of ~S)" type +coercion-types+))
  (unless (or (null value) (stringp value))
    (%canvas-error "~A: a request value must be a string or NIL, not ~S" field value))
  (when (eq type :choice)
    (unless (and (%proper-list-p choices)
                 (every (lambda (o) (stringp (%option-value o))) choices))
      (%canvas-error "~A: choices must be strings or (value . label) conses: ~S" field choices)))
  (flet ((fail (expected &optional detail)
           (restart-case (error 'canvas-coercion-error :field field :value value
                                                       :expected expected :detail detail)
             (use-value (new-value)
               :report "Use a value instead of the invalid one."
               new-value)
             (use-default ()
               :report "Use the field's default."
               default))))
    (let ((trimmed (and value (string-trim '(#\Space #\Tab) value))))
      (if (or (null trimmed) (zerop (length trimmed)))
          (if required (fail :required) default)
          (multiple-value-bind (result ok) (%coerce-typed type value trimmed choices)
            (cond ((not ok)
                   (fail type (when (eq type :choice) (mapcar #'%option-value choices))))
                  ((and min (realp result) (< result min)) (fail :min min))
                  ((and max (realp result) (> result max)) (fail :max max))
                  (t result)))))))

(defun coerce-form (params specs)
  "Coerce a param plist against SPECS -> (values plist errors)."
  (unless (%plist-p params)
    (%canvas-error "params must be a plist: ~S" params))
  (let ((plist '())
        (errors '()))
    (dolist (spec specs)
      (unless (and (consp spec) (keywordp (first spec)) (%plist-p (cddr spec)))
        (%canvas-error "invalid form spec ~S (want (key type . keys))" spec))
      (destructuring-bind (key type &rest keys) spec
        (let ((value (handler-bind ((canvas-coercion-error
                                      (lambda (c)
                                        (push c errors)
                                        (invoke-restart (find-restart 'use-default c)))))
                       (apply #'coerce-param (getf params key) type
                              (append keys (list :field (string-downcase (symbol-name key))))))))
          (setf plist (list* value key plist)))))
    (values (nreverse plist) (nreverse errors))))

(defun find-field-error (errors field)
  (find field errors :key #'coercion-error-field :test #'string=))
