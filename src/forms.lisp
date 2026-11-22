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

(defun error-fragment (condition)
  "<small id=\"<field>-error\" role=\"alert\">text</small> for a CANVAS-COERCION-ERROR."
  (declare (ignore condition))
  (%h4h0-pending 'error-fragment))

(defun coerce-param (value type &key (field "value") default required min max choices)
  "Coerce the request string VALUE to TYPE. See the file header."
  (declare (ignore value type field default required min max choices))
  (%h4h0-pending 'coerce-param))

(defun coerce-form (params specs)
  "Coerce a param plist against SPECS -> (values plist errors)."
  (declare (ignore params specs))
  (%h4h0-pending 'coerce-form))

(defun find-field-error (errors field)
  (declare (ignore errors field))
  (%h4h0-pending 'find-field-error))
