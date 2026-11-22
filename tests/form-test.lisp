(in-package #:canvas-protocol/tests)

;;; Inputs

(deftest text-input-html
  (ok (string= "<label>Name<input type=\"text\" name=\"name\" value=\"A&amp;B\" placeholder=\"you\" required=\"\"></label>"
               (html (text-input "name" :label "Name" :value "A&B" :placeholder "you" :required t))))
  (ok (string= "<label><input type=\"text\" name=\"q\"></label>" (html (text-input "q"))))
  (ok (string= "<label>&lt;L&gt;<input type=\"text\" name=\"q\" value=\"&quot;>&lt;x\"></label>"
               (html (text-input "q" :label "<L>" :value "\"><x"))))
  (ok (signals (text-input "bad name") 'canvas-error))
  (ok (signals (text-input "a\"b") 'canvas-error)))

(deftest number-and-date-input-html
  (ok (string= "<label><input type=\"number\" name=\"n\" value=\"3\" min=\"0\" max=\"10\" step=\"1\"></label>"
               (html (number-input "n" :value 3 :min 0 :max 10 :step 1))))
  (ok (string= "<label>When<input type=\"date\" name=\"d\" value=\"2026-11-22\"></label>"
               (html (date-input "d" :label "When" :value '(2026 11 22)))))
  (ok (string= "<label><input type=\"date\" name=\"d\" value=\"2026-01-05\" min=\"2026-01-01\" required=\"\"></label>"
               (html (date-input "d" :value "2026-01-05" :min "2026-01-01" :required t)))))

(deftest checkbox-html
  (ok (string= "<label><input type=\"checkbox\" name=\"agree\" value=\"on\" checked=\"\">I agree</label>"
               (html (checkbox "agree" :label "I agree" :checked t))))
  (ok (string= "<label><input type=\"checkbox\" name=\"x\" value=\"yes\"></label>"
               (html (checkbox "x" :value "yes")))))

(deftest select-input-html
  (ok (string= (s+ "<label>Colour<select name=\"c\"><option value=\"r\">Red</option>"
                   "<option value=\"g\" selected=\"\">Green</option></select></label>")
               (html (select-input "c" '(("r" . "Red") ("g" . "Green")) :value "g" :label "Colour"))))
  (ok (string= (s+ "<label><select name=\"s\" required=\"\"><option value=\"a&amp;b\">a&amp;b</option>"
                   "<option value=\"c\">c</option></select></label>")
               (html (select-input "s" '("a&b" "c") :required t)))))

(deftest textarea-html
  (ok (string= "<label><textarea name=\"t\" rows=\"3\">&lt;/textarea&gt;&lt;b&gt;</textarea></label>"
               (html (textarea "t" :value "</textarea><b>" :rows 3))))
  (ok (string= "<label>Notes<textarea name=\"t\" placeholder=\"...\"></textarea></label>"
               (html (textarea "t" :label "Notes" :placeholder "...")))))

(deftest submit-button-html
  (ok (string= "<button type=\"submit\">Save</button>" (html (submit-button "Save"))))
  (ok (string= "<button type=\"submit\" name=\"op\" value=\"del\">Delete &lt;x&gt;</button>"
               (html (submit-button "Delete <x>" :name "op" :value "del")))))

(deftest form-html
  (ok (string= (s+ "<form id=\"f\" hx-post=\"" +u+ "SAVE\" hx-target=\"#f\" hx-swap=\"outerHTML\">"
                   "<label><input type=\"text\" name=\"q\"></label><button type=\"submit\">Go</button></form>")
               (html (form (list (text-input "q") (submit-button "Go"))
                           :link (canvas-link 'save) :id "f"))))
  (ok (string= (s+ "<form hx-post=\"" +u+ "SAVE?k=1\">x</form>")
               (html (form "x" :link (canvas-link 'save :k 1)))))
  (ok (string= (s+ "<form id=\"f\" hx-post=\"" +u+ "SAVE\" hx-target=\"#out\" hx-swap=\"innerHTML\">x</form>")
               (html (form "x" :link (canvas-link 'save) :id "f" :target "#out" :swap :inner-html))))
  (ok (string= "<form id=\"f\">x</form>"
               (let ((*canvas-resolver* (make-freeze-resolver)))
                 (to-html (form "x" :link (canvas-link 'save) :id "f"))))))

(deftest input-with-error
  (let ((c (coercion-error "abc" :integer :field "age")))
    (ok (string= "<small id=\"age-error\" role=\"alert\">age must be an integer</small>"
                 (html (error-fragment c))))
    (ok (string= (html (error-fragment c)) (html c)) "render of the condition")
    (ok (string= (s+ "<label>Age<input type=\"text\" name=\"age\" value=\"abc\" aria-invalid=\"true\""
                     " aria-describedby=\"age-error\"><small id=\"age-error\" role=\"alert\">age must be an integer</small></label>")
                 (html (text-input "age" :label "Age" :value "abc" :error c))))))

;;; Coercion

(deftest coerce-strings
  (ok (string= "x" (coerce-param "x" :string)))
  (ok (string= " x " (coerce-param " x " :string)))
  (ok (string= "d" (coerce-param "" :string :default "d")))
  (ok (null (coerce-param nil :string)))
  (ok (signals (coerce-param "" :string :required t) 'canvas-coercion-error))
  (ok (signals (coerce-param "  " :string :required t) 'canvas-coercion-error)))

(deftest coerce-integers
  (ok (eql 42 (coerce-param "42" :integer)))
  (ok (eql -7 (coerce-param " -7 " :integer)))
  (ok (eql 3 (coerce-param "+3" :integer)))
  (ok (eql 12345678901234567890 (coerce-param "12345678901234567890" :integer)))
  (ok (eql 9 (coerce-param "" :integer :default 9)))
  (ok (null (coerce-param nil :integer)))
  (dolist (bad '("1.5" "abc" "0x1F" "1 2" "-" "#.(error \"read\")" "#x10" "1e3"))
    (ok (signals (coerce-param bad :integer) 'canvas-coercion-error) (format nil "~S" bad))))

(deftest coerce-bounds
  (let ((c (coercion-error "5" :integer :field "n" :min 6)))
    (ok (eq :min (coercion-error-expected c)))
    (ok (eql 6 (coercion-error-detail c)))
    (ok (string= "n must be at least 6" (princ-to-string c))))
  (let ((c (coercion-error "5" :integer :field "n" :max 4)))
    (ok (eq :max (coercion-error-expected c)))
    (ok (string= "n must be at most 4" (princ-to-string c))))
  (ok (eql 6 (coerce-param "6" :integer :min 6 :max 6)))
  (ok (signals (coerce-param "0.5" :float :min 1) 'canvas-coercion-error)))

(deftest coerce-floats
  (ok (eql 1.5d0 (coerce-param "1.5" :float)))
  (ok (eql -2.0d0 (coerce-param "-2" :float)))
  (ok (eql 0.5d0 (coerce-param ".5" :float)))
  (ok (eql 1000.0d0 (coerce-param "1e3" :float)))
  (ok (eql 0.125d0 (coerce-param "1.25E-1" :float)))
  (dolist (bad '("1.2.3" "NaN" "1/2" "e3" "1e" "inf" "#.(error \"read\")"))
    (ok (signals (coerce-param bad :float) 'canvas-coercion-error) (format nil "~S" bad))))

(deftest coerce-booleans
  (dolist (v '("on" "true" "1" "yes" "TRUE" "On"))
    (ok (eq t (coerce-param v :boolean)) v))
  (dolist (v '("off" "false" "0" "no"))
    (ok (null (coerce-param v :boolean)) v))
  (ok (null (coerce-param nil :boolean)))
  (ok (signals (coerce-param "maybe" :boolean) 'canvas-coercion-error)))

(deftest coerce-dates
  (ok (equal '(2026 11 22) (coerce-param "2026-11-22" :date)))
  (ok (equal '(2024 2 29) (coerce-param "2024-02-29" :date)))
  (dolist (bad '("2026-02-29" "2026-13-01" "2026-00-10" "2026-04-31" "22/11/2026" "2026-1-5" "1900-02-29"))
    (ok (signals (coerce-param bad :date) 'canvas-coercion-error) bad)))

(deftest coerce-choices
  (ok (string= "g" (coerce-param "g" :choice :choices '("r" "g"))))
  (ok (string= "r" (coerce-param "r" :choice :choices '(("r" . "Red")))))
  (let ((c (coercion-error "x" :choice :field "c" :choices '("r" "g"))))
    (ok (equal '("r" "g") (coercion-error-detail c)))
    (ok (string= "c must be one of: r, g" (princ-to-string c)))))

(deftest coerce-unknown-type
  (ok (typep (handler-case (coerce-param "1" :complex) (error (c) c)) 'canvas-error))
  (ok (not (typep (handler-case (coerce-param "1" :complex) (error (c) c)) 'canvas-coercion-error))))

(deftest coercion-condition-slots-and-text
  (let ((c (coercion-error "abc" :integer :field "age")))
    (ok (typep c 'canvas-error))
    (ok (string= "age" (coercion-error-field c)))
    (ok (string= "abc" (coercion-error-value c)))
    (ok (eq :integer (coercion-error-expected c))))
  (ok (string= "value must be an integer" (princ-to-string (coercion-error "x" :integer))))
  (ok (string= "name is required" (princ-to-string (coercion-error "" :string :field "name" :required t))))
  (ok (string= "x must be a number" (princ-to-string (coercion-error "z" :float :field "x"))))
  (ok (string= "x must be on or off" (princ-to-string (coercion-error "z" :boolean :field "x"))))
  (ok (string= "x must be a date (YYYY-MM-DD)" (princ-to-string (coercion-error "z" :date :field "x")))))

(deftest coercion-restarts
  (ok (eql 7 (handler-bind ((canvas-coercion-error
                              (lambda (c) (declare (ignore c)) (invoke-restart 'use-value 7))))
               (coerce-param "abc" :integer))))
  (ok (eql 3 (handler-bind ((canvas-coercion-error
                              (lambda (c) (declare (ignore c)) (invoke-restart 'use-default))))
               (coerce-param "abc" :integer :default 3))))
  (let ((names '()))
    (handler-case
        (handler-bind ((canvas-coercion-error
                         (lambda (c)
                           (setf names (mapcar #'restart-name (compute-restarts c))))))
          (coerce-param "abc" :integer))
      (canvas-coercion-error () nil))
    (ok (member 'use-value names))
    (ok (member 'use-default names))))

(deftest coerce-form-collects
  (multiple-value-bind (plist errors)
      (coerce-form '(:age "x" :name "Ann" :n "3" :extra "ignored")
                   '((:age :integer :default 0)
                     (:name :string :required t)
                     (:n :integer :max 2)
                     (:flag :boolean)
                     (:when :date :field "When")))
    (ok (equal '(:age 0 :name "Ann" :n nil :flag nil :when nil) plist))
    (ok (= 2 (length errors)))
    (ok (equal '("age" "n") (mapcar #'coercion-error-field errors)))
    (ok (eq (second errors) (find-field-error errors "n")))
    (ok (null (find-field-error errors "name"))))
  (multiple-value-bind (plist errors) (coerce-form '() '((:when :date :field "When" :required t)))
    (ok (equal '(:when nil) plist))
    (ok (string= "When is required" (princ-to-string (first errors)))))
  (multiple-value-bind (plist errors) (coerce-form '(:n "5") '((:n :integer)))
    (ok (equal '(:n 5) plist))
    (ok (null errors))))
