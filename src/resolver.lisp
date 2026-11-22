(in-package #:canvas-protocol)

;;; URLs. A canvas is a function named by a symbol; a CANVAS-LINK is that
;;; symbol plus request params. Resolvers turn links and asset names into URL
;;; strings (or NIL = no URL: the element renders inert).
;;;
;;; Param normalization (CANVAS-LINK, LINK-URL extras, :vals):
;;; - a plist; odd length -> CANVAS-ERROR;
;;; - key: keyword or symbol -> lowercased name;
;;; - value: string as is; integer decimal; T -> "true"; NIL -> pair dropped;
;;;   other keyword/symbol -> lowercased name; anything else -> CANVAS-ERROR;
;;; - result: alist (name . string) in first-occurrence order; a later
;;;   duplicate replaces the earlier value in place.
;;; CANVAS-LINK's function must be a non-keyword symbol that is FBOUNDP,
;;; else CANVAS-ERROR. LINK-URL extras merge over the link's params the same way.
;;;
;;; PATH-RESOLVER URL: <prefix><enc package-name>::<enc symbol-name>[?<k>=<v>&...]
;;; with RFC 3986 percent-encoding of the UTF-8 bytes of everything outside
;;; A-Z a-z 0-9 - . _ ~ (uppercase hex; space -> %20). Asset URL: <asset-prefix><name>.
;;; FREEZE-RESOLVER: NIL for both.

(defvar *canvas-resolver* nil
  "The resolver CANVAS-URL, LINK-URL, the hx helpers and PAGE use. NIL -> CANVAS-NO-RESOLVER.")

(defclass canvas-resolver () ()
  (:documentation "Base class. Servers (rpc-backend-htmx/canvas), tests and freeze subclass it."))

(defgeneric resolve-canvas-url (resolver symbol params)
  (:documentation "URL string (or NIL) for calling the canvas function SYMBOL with PARAMS,
a normalized alist of (name . string)."))

(defgeneric resolve-asset-url (resolver name)
  (:documentation "URL string (or NIL) for the asset NAME (see ASSET-NAMES)."))

(defclass path-resolver (canvas-resolver)
  ((prefix :initarg :prefix :initform "/canvas/" :reader path-resolver-prefix)
   (asset-prefix :initarg :asset-prefix :initform "/canvas-assets/"
                 :reader path-resolver-asset-prefix))
  (:documentation "<prefix><PACKAGE>::<NAME>?k=v -- the H8d-1 shape with prefix /canvas/<session>/."))

(defun make-path-resolver (&key (prefix "/canvas/") (asset-prefix "/canvas-assets/"))
  (check-type prefix string)
  (check-type asset-prefix string)
  (make-instance 'path-resolver :prefix prefix :asset-prefix asset-prefix))

(defun %resolver ()
  (or *canvas-resolver* (error 'canvas-no-resolver)))

(defparameter +hex-digits+ "0123456789ABCDEF")

(defun %unreserved-octet-p (octet)
  (let ((c (code-char octet)))
    (or (%ascii-alnum-p c) (find c "-._~"))))

(defun %percent-encode (string)
  "RFC 3986 percent-encoding of the UTF-8 bytes of STRING (uppercase hex)."
  (with-output-to-string (out)
    (loop for octet across (stack-encoding:encode-to-octets string :encoding :utf-8)
          do (if (%unreserved-octet-p octet)
                 (write-char (code-char octet) out)
                 (progn (write-char #\% out)
                        (write-char (char +hex-digits+ (ash octet -4)) out)
                        (write-char (char +hex-digits+ (logand octet 15)) out))))))

(defun %query-string (params)
  (format nil "~{~A~^&~}"
          (mapcar (lambda (p) (%str (%percent-encode (car p)) "=" (%percent-encode (cdr p))))
                  params)))

(defmethod resolve-canvas-url ((resolver path-resolver) symbol params)
  (%str (path-resolver-prefix resolver)
        (%percent-encode (package-name (symbol-package symbol)))
        "::"
        (%percent-encode (symbol-name symbol))
        (if params (%str "?" (%query-string params)) "")))

(defmethod resolve-asset-url ((resolver path-resolver) name)
  (%str (path-resolver-asset-prefix resolver) name))

(defclass freeze-resolver (canvas-resolver) ()
  (:documentation "Used by FREEZE: no canvas or asset has a URL."))

(defun make-freeze-resolver ()
  (make-instance 'freeze-resolver))

(defmethod resolve-canvas-url ((resolver freeze-resolver) symbol params)
  (declare (ignore symbol params))
  nil)

(defmethod resolve-asset-url ((resolver freeze-resolver) name)
  (declare (ignore name))
  nil)

(defclass canvas-link ()
  ((function :initarg :function :reader canvas-link-function)
   (params :initarg :params :initform nil :reader canvas-link-params
           :documentation "Normalized alist of (name . string)."))
  (:documentation "A canvas function symbol plus request params. Made by CANVAS-LINK."))

(defun canvas-link-p (object)
  (typep object 'canvas-link))

(defun %param-value (key value)
  (cond ((stringp value) value)
        ((integerp value) (format nil "~D" value))
        ((eq value t) "true")
        ((symbolp value) (string-downcase (symbol-name value)))
        (t (%canvas-error "param ~A: value must be a string, integer or symbol, not ~S"
                          key value))))

(defun %normalize-params (plist &optional base)
  "Param PLIST merged over the normalized alist BASE -> a fresh alist (name . string)."
  (unless (%plist-p plist)
    (%canvas-error "params must be a plist: ~S" plist))
  (let ((acc (copy-alist base)))
    (loop for (key value) on plist by #'cddr
          for name = (if (and key (symbolp key))
                         (string-downcase (symbol-name key))
                         (%canvas-error "param name must be a symbol: ~S" key))
          when value
            do (let ((string (%param-value name value))
                     (cell (assoc name acc :test #'string=)))
                 (if cell
                     (setf (cdr cell) string)
                     (setf acc (append acc (list (cons name string)))))))
    acc))

(defun canvas-link (function &rest params)
  "Link to the canvas FUNCTION (an fbound symbol) with PARAMS (a plist)."
  (unless (and function (symbolp function) (not (keywordp function))
               (symbol-package function) (fboundp function))
    (%canvas-error "a canvas must be an fbound, interned, non-keyword symbol: ~S" function))
  (make-instance 'canvas-link :function function :params (%normalize-params params)))

(defun %link-merge (link &rest params)
  "A new CANVAS-LINK: LINK's function with PARAMS merged over its params."
  (unless (canvas-link-p link)
    (%canvas-error "not a canvas-link: ~S" link))
  (make-instance 'canvas-link
                 :function (canvas-link-function link)
                 :params (%normalize-params params (canvas-link-params link))))

(defun link-url (link &rest params)
  "Resolve LINK (with PARAMS merged over its own) through *CANVAS-RESOLVER*."
  (let ((merged (apply #'%link-merge link params)))
    (resolve-canvas-url (%resolver) (canvas-link-function merged) (canvas-link-params merged))))

(defun canvas-url (function &rest params)
  "(link-url (apply #'canvas-link function params))."
  (link-url (apply #'canvas-link function params)))
