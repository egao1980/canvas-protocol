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

(defmethod resolve-canvas-url ((resolver path-resolver) symbol params)
  (declare (ignore symbol params))
  (%h4h0-pending 'resolve-canvas-url))

(defmethod resolve-asset-url ((resolver path-resolver) name)
  (declare (ignore name))
  (%h4h0-pending 'resolve-asset-url))

(defclass freeze-resolver (canvas-resolver) ()
  (:documentation "Used by FREEZE: no canvas or asset has a URL."))

(defun make-freeze-resolver ()
  (make-instance 'freeze-resolver))

(defmethod resolve-canvas-url ((resolver freeze-resolver) symbol params)
  (declare (ignore symbol params))
  (%h4h0-pending 'resolve-canvas-url))

(defmethod resolve-asset-url ((resolver freeze-resolver) name)
  (declare (ignore name))
  (%h4h0-pending 'resolve-asset-url))

(defclass canvas-link ()
  ((function :initarg :function :reader canvas-link-function)
   (params :initarg :params :initform nil :reader canvas-link-params
           :documentation "Normalized alist of (name . string)."))
  (:documentation "A canvas function symbol plus request params. Made by CANVAS-LINK."))

(defun canvas-link-p (object)
  (typep object 'canvas-link))

(defun canvas-link (function &rest params)
  "Link to the canvas FUNCTION (an fbound symbol) with PARAMS (a plist)."
  (declare (ignore function params))
  (%h4h0-pending 'canvas-link))

(defun link-url (link &rest params)
  "Resolve LINK (with PARAMS merged over its own) through *CANVAS-RESOLVER*."
  (declare (ignore link params))
  (%h4h0-pending 'link-url))

(defun canvas-url (function &rest params)
  "(link-url (apply #'canvas-link function params))."
  (declare (ignore function params))
  (%h4h0-pending 'canvas-url))
