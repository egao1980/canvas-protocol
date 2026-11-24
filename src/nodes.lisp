(in-package #:canvas-protocol)

;;; Trees are html-protocol nodes (stack-html). Components render into them;
;;; raw nodes pass through RENDER unchanged.
;;;
;;; Rules (H4h-0, see cl-stack/docs/capabilities/canvas.md):
;;; - EL tag: keyword or string, lowercased, [a-z][a-z0-9-]*; else CANVAS-ERROR.
;;;   Stored as STACK-HTML:CANONICAL-ELEMENT-NAME (SVG camelCase tags such as
;;;   :lineargradient -> "linearGradient").
;;;   Refused tags (CANVAS-CSP-ERROR): script iframe object embed.
;;; - EL attrs: a plist. Name = lowercased symbol name, [a-z][a-z0-9:._-]*, else
;;;   CANVAS-ERROR, stored as STACK-HTML:CANONICAL-ATTRIBUTE-NAME (SVG camelCase
;;;   names such as :viewbox -> "viewBox"; every CSP check folds case); an odd plist is a CANVAS-ERROR. Value: string as is, integer
;;;   decimal, T -> "" (boolean attribute), NIL -> attribute omitted; anything
;;;   else CANVAS-ERROR. Output order = the order given (html-protocol 0.1.1
;;;   guarantees attributes print in list order on every implementation).
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
;;;   fragment: html-protocol 0.1.1 guarantees every text node is escaped and
;;;   attribute values escape &, " and <, so TO-HTML escapes nothing by hand.

(defgeneric render (object)
  (:documentation "OBJECT -> an html-protocol node, a list of nodes, or NIL."))

;;; Shared predicates. Character classes are explicit ASCII ranges (never
;;; ALPHA-CHAR-P, which accepts non-ASCII letters on every implementation).

(defun %ascii-lower-p (char) (char<= #\a char #\z))
(defun %ascii-upper-p (char) (char<= #\A char #\Z))
(defun %ascii-digit-p (char) (char<= #\0 char #\9))
(defun %ascii-alnum-p (char)
  (or (%ascii-lower-p char) (%ascii-upper-p char) (%ascii-digit-p char)))

(defun %token-p (string first-ok rest-ok)
  "STRING is non-empty, its first char satisfies FIRST-OK and the rest REST-OK."
  (and (stringp string)
       (plusp (length string))
       (funcall first-ok (char string 0))
       (every rest-ok (subseq string 1))))

(defun %str (&rest parts)
  (apply #'concatenate 'string parts))

(defun %prefix-p (prefix string)
  "STRING starts with PREFIX (case-insensitive)."
  (and (>= (length string) (length prefix))
       (string-equal prefix string :end2 (length prefix))))

(defun %plist-p (object)
  "OBJECT is a proper list of even length."
  (loop (cond ((null object) (return t))
              ((and (consp object) (consp (cdr object))) (setf object (cddr object)))
              (t (return nil)))))

(defun %proper-list-p (object)
  "OBJECT is a proper (NIL-terminated) list."
  (loop (cond ((null object) (return t))
              ((consp object) (setf object (cdr object)))
              (t (return nil)))))

(defun %control-char-p (char)
  "CHAR is a Unicode control character (general category Cc: codes 0-31, 127-159)."
  (let ((code (char-code char)))
    (or (< code 32) (<= 127 code 159))))

(defun %remove-controls (string &optional (keep ""))
  "STRING without its control characters other than those in KEEP; STRING itself
(no copy) when there is nothing to remove."
  (flet ((drop-p (c) (and (%control-char-p c) (not (find c keep)))))
    (if (find-if #'drop-p string) (remove-if #'drop-p string) string)))

(defun %split (string char)
  "STRING split at every CHAR -> fresh substrings in order, always at least one:
\"\" -> (\"\"), \"a,,b\" -> (\"a\" \"\" \"b\") (UIOP:SPLIT-STRING gives NIL for \"\")."
  (loop for start = 0 then (1+ end)
        for end = (position char string :start start)
        collect (subseq string start end)
        while end))

(defun %canvas-error (control &rest args)
  (error 'canvas-error :message (apply #'format nil control args)))

;;; CSP policy: one classifier for attributes and one for elements, shared by
;;; EL (refusal), CSP-VIOLATIONS (scan), PAGE's STRIP-VIOLATIONS and NEUTRALIZE.

(defparameter +url-attributes+
  '("href" "src" "action" "formaction" "srcset" "poster" "xlink:href"
    "hx-get" "hx-post" "hx-put" "hx-patch" "hx-delete" "sse-connect"
    "data-hx-get" "data-hx-post" "data-hx-put" "data-hx-patch" "data-hx-delete"
    "data-sse-connect")
  "Attributes whose value is a URL (checked for javascript:).")

(defun %url-scheme-text (value)
  "VALUE as a browser reads a URL scheme: leading C0/space stripped, tab/CR/LF removed."
  (remove-if (lambda (c) (member c '(#\Tab #\Newline #\Return)))
             (string-left-trim (loop for i from 0 to 32 collect (code-char i)) value)))

(defun %javascript-url-p (value)
  (and (stringp value) (%prefix-p "javascript:" (%url-scheme-text value))))

(defun %attribute-violation (name value)
  "CSP violation KIND of the attribute NAME (any case) with string VALUE, or NIL."
  (let ((name (string-downcase name))
        (value (if (stringp value) value (princ-to-string value))))
    (cond ((or (%prefix-p "hx-on" name) (%prefix-p "data-hx-on" name)) :hx-on)
          ((%prefix-p "on" name) :event-handler)
          ((string= name "style") :inline-style)
          ((member name '("hx-vars" "data-hx-vars") :test #'string=) :js-vals)
          ((and (member name '("hx-vals" "data-hx-vals") :test #'string=)
                (let ((v (string-left-trim '(#\Space #\Tab #\Newline #\Return #\Page) value)))
                  (or (%prefix-p "js:" v) (%prefix-p "javascript:" v))))
           :js-vals)
          ((and (member name '("hx-trigger" "data-hx-trigger") :test #'string=)
                (find #\[ value))
           :eval-trigger)
          ((and (member name +url-attributes+ :test #'string=) (%javascript-url-p value))
           :javascript-url))))

(defun %node-name (name)
  (string-downcase (string name)))

(defun %element-violation (element)
  "CSP violation KIND of the html-element ELEMENT itself (not its attributes), or NIL."
  (let ((name (%node-name (stack-html:html-element-name element))))
    (cond ((string= name "script")
           (when (or (stack-html:html-element-children element)
                     (not (stack-html:element-attr element "src")))
             :inline-script))
          ((member name '("iframe" "object" "embed") :test #'string=) :embed))))

(defun %csp-error (kind element attribute)
  (let ((v (list :kind kind :element element :attribute attribute)))
    (error 'canvas-csp-error
           :violations (list v)
           :message (format nil "CSP: ~(~A~)~@[ in <~A>~]~@[ attribute ~A~]"
                            kind element attribute))))

;;; EL

(defun %tag-name (tag)
  (let ((name (cond ((stringp tag) (string-downcase tag))
                    ((keywordp tag) (string-downcase (symbol-name tag)))
                    (t (%canvas-error "element name must be a keyword or string: ~S" tag)))))
    (unless (%token-p name #'%ascii-lower-p
                      (lambda (c) (or (%ascii-lower-p c) (%ascii-digit-p c) (char= c #\-))))
      (%canvas-error "invalid element name ~S" tag))
    (cond ((string= name "script") (%csp-error :inline-script name nil))
          ((member name '("iframe" "object" "embed") :test #'string=)
           (%csp-error :embed name nil)))
    (stack-html:canonical-element-name name)))

(defun %attr-name (key)
  (let ((name (and key (symbolp key) (string-downcase (symbol-name key)))))
    (unless (%token-p name #'%ascii-lower-p
                      (lambda (c) (or (%ascii-lower-p c) (%ascii-digit-p c) (find c ":._-"))))
      (%canvas-error "invalid attribute name ~S" key))
    (stack-html:canonical-attribute-name name)))

(defun %attr-value (name value)
  (cond ((stringp value) value)
        ((integerp value) (format nil "~D" value))
        ((eq value t) "")
        ((null value) nil)
        (t (%canvas-error "attribute ~A: value must be a string, integer, T or NIL, not ~S"
                          name value))))

(defun %attr-alist (plist &optional (element "element"))
  "Attribute PLIST -> alist (name . string) in the given order, with CSP refusals."
  (unless (%plist-p plist)
    (%canvas-error "attributes must be a plist: ~S" plist))
  (loop for (key value) on plist by #'cddr
        for name = (%attr-name key)
        for string = (%attr-value name value)
        when string
          do (let ((kind (%attribute-violation name string)))
               (when kind (%csp-error kind element name)))
          and collect (cons name string)))

(defun %nodes (rendered)
  "A RENDER result -> a list of nodes."
  (if (listp rendered) rendered (list rendered)))

(defun %text (string)
  (make-instance 'stack-html:html-text :data string))

;;; RENDER

(defmethod render ((object t))
  (%canvas-error "cannot render ~S (no RENDER method for ~S)" object (type-of object)))

(defmethod render ((object stack-html:html-node))
  object)

(defmethod render ((object string))
  (%text object))

(defmethod render ((object real))
  (%text (let ((*print-base* 10) (*print-radix* nil))
           (princ-to-string object))))

(defmethod render ((object null))
  nil)

(defmethod render ((object list))
  (loop for item in object
        for r = (render item)
        if (listp r) append r
        else collect r))

(defun to-html (object)
  "Serialize (RENDER OBJECT) to a string. Documents include their doctype."
  (let ((r (render object)))
    (cond ((null r) "")
          ((stack-html:html-document-p r) (stack-html:serialize r))
          (t (stack-html:serialize (stack-html:make-html-document :children (%nodes r)))))))

(defun el (tag attrs &rest children)
  "Build an html-protocol element. (el :p nil \"hi\"), (el :a '(:href \"#x\") \"x\")."
  (let ((name (%tag-name tag)))
    (stack-html:make-html-element name :attributes (%attr-alist attrs name)
                                       :children (render children))))

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

(defmethod validate-component :before ((component component))
  (let ((id (component-id component)))
    (unless (or (null id) (stringp id))
      (%canvas-error "component id must be a string: ~S" id)))
  ;; Caller attributes follow EL's rules (CSP refusals included) at construction.
  (%attr-alist (component-attrs component)))

(defmethod render ((object component))
  (%canvas-error "no RENDER method for the component ~S" (type-of object)))

(defun %own (component &rest own)
  "Attribute plist for COMPONENT's root: id, then OWN, then the caller's attrs."
  (append (list :id (component-id component)) own (component-attrs component)))

(defun %make (class &rest initargs)
  (validate-component (apply #'make-instance class initargs)))
