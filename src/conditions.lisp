(in-package #:canvas-protocol)

;;; Conditions (CLHS 9). Every error canvas-protocol signals on purpose is a
;;; CANVAS-ERROR. Restart names exported: USE-DEFAULT (coerce-param),
;;; STRIP-VIOLATIONS (page, svg-fragment), USE-PLACEHOLDER (render of a chart);
;;; CL:USE-VALUE is offered by coerce-param, chart-node and the asset lookups.
;;;
;;; 0.2.0 (HX2) adds UNSAFE-SVG (svg-fragment, see svg.lisp), the :file
;;; coercion type with the expectations :file :max-size :accept, and
;;; CANVAS-UPLOAD-TOO-LARGE (forms.lisp).

(define-condition h4h0-pending (error)
  ((name :initarg :name :reader h4h0-pending-name))
  (:report (lambda (c s)
             (format s "H4h-0 pending: ~A is not implemented yet" (h4h0-pending-name c)))))

(defun %h4h0-pending (name)
  "Stub marker for the H4h-0 team-solve run. Not a CANVAS-ERROR on purpose."
  (error 'h4h0-pending :name name))

(define-condition canvas-error (error)
  ((message :initarg :message :initform nil :reader canvas-error-message))
  (:report (lambda (c s)
             (format s "Canvas error~@[: ~A~]" (canvas-error-message c)))))

(define-condition canvas-no-resolver (canvas-error) ()
  (:default-initargs
   :message "*canvas-resolver* is NIL: bind a path-resolver, a freeze-resolver or the server's resolver"))

(define-condition canvas-csp-error (canvas-error)
  ((violations :initarg :violations :initform nil :reader csp-error-violations))
  (:documentation "Output that would break the H8d-1 CSP (script-src 'self', style-src 'self').
VIOLATIONS is a list of plists as returned by CSP-VIOLATIONS."))

(define-condition unsafe-svg (canvas-csp-error) ()
  (:documentation "SVG-FRAGMENT found content outside its allowlist (svg.lisp). VIOLATIONS
(CSP-ERROR-VIOLATIONS) lists each offending element or attribute in document
order as (:kind KIND :element LOCAL-NAME :attribute QNAME-or-NIL); KIND is one of
:inline-script :foreign-object :element :event-handler :htmx-attribute :attribute
:external-href :css-url. Signalled with ERROR inside the restart STRIP-VIOLATIONS."))

(define-condition canvas-asset-not-found (canvas-error)
  ((name :initarg :name :reader asset-error-name))
  (:report (lambda (c s)
             (format s "Canvas asset not found: ~S" (asset-error-name c)))))

;;; Form coercion

(defparameter +coercion-types+ '(:string :integer :float :boolean :date :choice :file)
  "Types COERCE-PARAM accepts. :file (0.2.0) takes uploaded parts, see forms.lisp.")

(define-condition canvas-coercion-error (canvas-error)
  ((field :initarg :field :initform "value" :reader coercion-error-field)
   (value :initarg :value :initform nil :reader coercion-error-value)
   (expected :initarg :expected :reader coercion-error-expected
             :documentation "One of :required :integer :float :boolean :date :choice :min :max,
or for :file (0.2.0) :file :max-size :accept.")
   (detail :initarg :detail :initform nil :reader coercion-error-detail
           :documentation "The bound for :min / :max / :max-size, the choice values for :choice,
the accept entries for :accept, else NIL."))
  (:report (lambda (c s) (write-string (coercion-error-text c) s))))

(defun coercion-error-text (condition)
  "The user-facing message of a coercion error: \"<field> <problem>\"."
  (let ((field (coercion-error-field condition))
        (detail (coercion-error-detail condition)))
    (ecase (coercion-error-expected condition)
      (:required (format nil "~A is required" field))
      (:integer (format nil "~A must be an integer" field))
      (:float (format nil "~A must be a number" field))
      (:boolean (format nil "~A must be on or off" field))
      (:date (format nil "~A must be a date (YYYY-MM-DD)" field))
      (:choice (format nil "~A must be one of: ~{~A~^, ~}" field detail))
      (:min (format nil "~A must be at least ~A" field detail))
      (:max (format nil "~A must be at most ~A" field detail))
      (:file (format nil "~A must be a file" field))
      (:max-size (format nil "~A must be at most ~D bytes" field detail))
      (:accept (format nil "~A must be a file of type: ~{~A~^, ~}" field detail)))))

(define-condition canvas-upload-too-large (canvas-coercion-error)
  ((size :initarg :size :reader upload-error-size
         :documentation "The uploaded file's size in octets."))
  (:default-initargs :expected :max-size)
  (:documentation "An uploaded file is larger than COERCE-PARAM's :max-size (the DETAIL).
Signalled like every coercion error: ERROR inside USE-VALUE and USE-DEFAULT."))

;;; Charts

(define-condition invalid-chart (canvas-error)
  ((field :initarg :field :reader invalid-chart-field
          :documentation "Keyword naming the offending part: :kind :series :x-axis :y-axis
:bins :width :height :legend :scale :min :max :ticks.")
   (reason :initarg :reason :initform "" :reader invalid-chart-reason))
  (:report (lambda (c s)
             (format s "Invalid chart ~(~A~): ~A" (invalid-chart-field c) (invalid-chart-reason c)))))

(define-condition chart-backend-missing (canvas-error)
  ((chart :initarg :chart :initform nil :reader chart-backend-missing-chart))
  (:default-initargs
   :message "no chart backend: bind *chart-backend* (e.g. load canvas-backend-svg) or pass :backend"))
