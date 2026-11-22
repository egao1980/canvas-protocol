(in-package #:canvas-protocol)

;;; Trees are html-protocol nodes (stack-html). Components render into them;
;;; raw nodes pass through RENDER unchanged.
;;;
;;; Rules (H4h-0, see cl-stack/docs/capabilities/canvas.md):
;;; - EL tag: keyword or string, lowercased, [a-z][a-z0-9-]*; else CANVAS-ERROR.
;;;   Refused tags (CANVAS-CSP-ERROR): script iframe object embed.
;;; - EL attrs: a plist. Name = lowercased symbol name, [a-z][a-z0-9:._-]*, else
;;;   CANVAS-ERROR; an odd plist is a CANVAS-ERROR. Value: string as is, integer
;;;   decimal, T -> "" (boolean attribute), NIL -> attribute omitted; anything
;;;   else CANVAS-ERROR. Output order = the order given (html-protocol's writer
;;;   prints its attribute list reversed; account for that).
;;;   Refused (CANVAS-CSP-ERROR): names starting "on", "hx-on", "data-hx-on";
;;;   "style"; "hx-vars"; an "hx-vals" value starting "js:"; an "hx-trigger"
;;;   containing "["; a URL attribute (href src action formaction hx-get hx-post
;;;   hx-put hx-patch hx-delete sse-connect) whose value, trimmed and
;;;   case-folded, starts "javascript:".
;;; - EL children: each passed through RENDER; lists flattened; NIL dropped.
;;; - RENDER: html-node -> itself; string -> html-text (escaped on output);
;;;   integer / ratio / float -> its PRINC text; NIL -> NIL; list -> flattened
;;;   list of rendered nodes; COMPONENT -> its tree; CANVAS-COERCION-ERROR ->
;;;   ERROR-FRAGMENT; CHART -> CHART-NODE under *CHART-BACKEND* (restarts
;;;   USE-VALUE backend, USE-PLACEHOLDER -> <figure data-chart="missing">
;;;   <figcaption>title</figcaption></figure>). Any other object -> CANVAS-ERROR.
;;; - TO-HTML: string of the rendered object. An html-document serializes with
;;;   its doctype. Text is always escaped, including text at the top level of a
;;;   fragment (html-protocol 0.1.0 writes document-level text raw:
;;;   https://github.com/egao1980/html-protocol/issues/3).

(defgeneric render (object)
  (:documentation "OBJECT -> an html-protocol node, a list of nodes, or NIL."))

(defmethod render ((object t))
  (%h4h0-pending 'render))

(defun to-html (object)
  "Serialize (RENDER OBJECT) to a string. Documents include their doctype."
  (declare (ignore object))
  (%h4h0-pending 'to-html))

(defun el (tag attrs &rest children)
  "Build an html-protocol element. (el :p nil \"hi\"), (el :a '(:href \"#x\") \"x\")."
  (declare (ignore tag attrs children))
  (%h4h0-pending 'el))

;;; Base component. Every component has an optional ID (first attribute when
;;; present) and ATTRS, a plist of extra attributes appended last (same rules as
;;; EL attrs, CSP refusals included).

(defclass component ()
  ((id :initarg :id :initform nil :reader component-id)
   (attrs :initarg :attrs :initform nil :reader component-attrs))
  (:documentation "Base class of canvas components. RENDER turns one into a tree."))

(defgeneric validate-component (component)
  (:documentation "Signal CANVAS-ERROR when COMPONENT's options are invalid; return it.
Called by every constructor."))

(defmethod validate-component ((component component))
  component)

(defun %make (class &rest initargs)
  (validate-component (apply #'make-instance class initargs)))
