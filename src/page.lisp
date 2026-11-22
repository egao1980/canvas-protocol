(in-package #:canvas-protocol)

;;; Page, CSP scan, neutralize, freeze.
;;;
;;; PAGE body &key (title "canvas") (lang "en") (stylesheet :link) (htmx t)
;;; -> html-document:
;;;   <!DOCTYPE html><html lang="LANG"><head><meta charset="utf-8">
;;;   <meta name="viewport" content="width=device-width, initial-scale=1">
;;;   [<meta name="htmx-config" content="JSON of +HTMX-CONFIG+">]       (htmx t)
;;;   <title>TITLE</title>
;;;   [<link rel="stylesheet" href="(resolve-asset-url r "canvas.css")">] (:link)
;;;   [<style>(canvas-stylesheet)</style>]                                (:inline)
;;;   [<script src="...htmx.min.js"></script><script src="...htmx-ext-sse.min.js"></script>] (htmx t)
;;;   </head><body>BODY</body></html>
;;; STYLESHEET :link, :inline or NIL. Asset URLs from *CANVAS-RESOLVER*
;;; (NIL resolver -> CANVAS-NO-RESOLVER). The rendered BODY is scanned first:
;;; any CSP-VIOLATIONS -> CANVAS-CSP-ERROR (violations) with restart
;;; STRIP-VIOLATIONS (remove offending elements / attributes and go on).
;;;
;;; +HTMX-CONFIG+ is an alist in output order in json-protocol's value mapping
;;; (T -> true, NIL -> false, integers). The meta content is
;;; (stack-json:encode +htmx-config+); no JSON encoder of our own
;;; (json-protocol 0.2.2 guarantees alist key order, nested alists included).
;;;
;;; CSP-VIOLATIONS tree -> list, in document order (TREE is RENDERed first; a
;;; document is scanned whole), of
;;;   (:kind KIND :element "tag" :attribute "name-or-NIL")
;;; KIND: :inline-script (a <script> with text content or without src),
;;; :embed (<iframe> <object> <embed>), :event-handler (on* attribute),
;;; :hx-on (hx-on* / data-hx-on* attribute), :javascript-url (a URL attribute
;;; starting javascript:), :inline-style (style attribute), :eval-trigger
;;; (hx-trigger / data-hx-trigger containing "["), :js-vals (hx-vals starting
;;; "js:", or hx-vars). A <script src> with no content is fine.
;;;
;;; NEUTRALIZE tree -> a fresh tree (the input is not modified) without:
;;; hx-* / data-hx-* / sse-* attributes; <script> <link> <iframe> <object>
;;; <embed> <style> elements; on* and style attributes; src href action
;;; formaction srcset poster xlink:href values unless they start "#" or "data:".
;;;
;;; FREEZE fn &key args title -> string. FN an fbound symbol or a function
;;; (else CANVAS-ERROR); ARGS a plist (odd -> CANVAS-ERROR). Calls FN with ARGS
;;; under a FREEZE-RESOLVER, renders, NEUTRALIZEs, and returns
;;;   <!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
;;;   <meta name="viewport" content="width=device-width, initial-scale=1">
;;;   <title>TITLE</title><style>(canvas-stylesheet)</style></head>
;;;   <body data-canvas-frozen="">...</body></html>
;;; TITLE default: the lowercased symbol name, or "canvas" for a function object.
;;; htmx is dropped, not disabled (decision: a frozen file has no server).
;;; Byte-deterministic for the same function, args and image state.

(defparameter +htmx-config+
  '(("allowEval" . nil)
    ("allowScriptTags" . nil)
    ("historyCacheSize" . 0)
    ("includeIndicatorStyles" . nil)
    ("refreshOnHistoryMiss" . t)
    ("selfRequestsOnly" . t))
  "htmx.config for CSP (no eval, no inline styles) and privacy (no history snapshots).")

;;; Tree walking. %COPY-TREE rebuilds a fresh tree (the input is never
;;; modified); ELEMENT-FILTER gets each html-element and returns
;;; (values keep-p attribute-alist).

(defun %copy-tree (tree element-filter &key (keep-comments t))
  (labels ((copy (node)
             (cond ((listp node) (loop for n in node for c = (copy n) when c collect c))
                   ((stack-html:html-document-p node)
                    (stack-html:make-html-document
                     :children (copy (stack-html:html-document-children node))))
                   ((stack-html:html-element-p node)
                    (multiple-value-bind (keep attributes) (funcall element-filter node)
                      (when keep
                        (stack-html:make-html-element
                         (%node-name (stack-html:html-element-name node))
                         :attributes attributes
                         :children (copy (stack-html:html-element-children node))))))
                   ((stack-html:html-text-p node) (%text (stack-html:html-text-data node)))
                   ((stack-html:html-comment-p node)
                    (when keep-comments
                      (make-instance 'stack-html:html-comment
                                     :data (stack-html:html-comment-data node))))
                   ((stack-html:html-doctype-p node)
                    (make-instance 'stack-html:html-doctype
                                   :name (stack-html:html-doctype-name node)))
                   (t node))))
    (copy tree)))

(defun %attribute-pairs (element)
  "ELEMENT's attributes as fresh (lowercased-name . string) pairs."
  (loop for (name . value) in (stack-html:html-element-attributes element)
        collect (cons (%node-name name) (if (stringp value) value (princ-to-string value)))))

(defun csp-violations (tree)
  (let ((found '()))
    (labels ((walk (node)
               (cond ((listp node) (mapc #'walk node))
                     ((stack-html:html-document-p node)
                      (mapc #'walk (stack-html:html-document-children node)))
                     ((stack-html:html-element-p node)
                      (let ((name (%node-name (stack-html:html-element-name node)))
                            (kind (%element-violation node)))
                        (when kind
                          (push (list :kind kind :element name :attribute nil) found))
                        (loop for (attribute . value) in (%attribute-pairs node)
                              for k = (%attribute-violation attribute value)
                              when k
                                do (push (list :kind k :element name :attribute attribute) found))
                        (mapc #'walk (stack-html:html-element-children node)))))))
      (walk (render tree)))
    (nreverse found)))

(defun %strip-violations (tree)
  "Fresh TREE without the elements and attributes CSP-VIOLATIONS reports."
  (%copy-tree tree
              (lambda (element)
                (if (%element-violation element)
                    nil
                    (values t (remove-if (lambda (pair) (%attribute-violation (car pair) (cdr pair)))
                                         (%attribute-pairs element)))))))

(defparameter +neutralized-elements+ '("script" "link" "iframe" "object" "embed" "style"))

(defparameter +neutralized-url-attributes+
  '("src" "href" "action" "formaction" "srcset" "poster" "xlink:href"))

(defun %offline-url-p (value)
  "A URL a frozen file may keep: a fragment, or a raster-image data: URL."
  (let ((v (%url-scheme-text value)))
    (or (and (plusp (length v)) (char= (char v 0) #\#))
        (some (lambda (prefix)
                (and (%prefix-p prefix v)
                     (> (length v) (length prefix))
                     (find (char v (length prefix)) ";,")))
              '("data:image/png" "data:image/jpeg" "data:image/gif" "data:image/webp")))))

(defun %neutralized-attribute-p (name value)
  (or (%prefix-p "hx-" name)
      (%prefix-p "data-hx-" name)
      (%prefix-p "sse-" name)
      (%prefix-p "data-sse-" name)
      (%attribute-violation name value)
      (and (member name +neutralized-url-attributes+ :test #'string=)
           (not (%offline-url-p value)))))

(defun neutralize (tree)
  (%copy-tree (render tree)
              (lambda (element)
                (if (or (member (%node-name (stack-html:html-element-name element))
                                +neutralized-elements+ :test #'string=)
                        (%element-violation element))
                    nil
                    (values t (remove-if (lambda (pair)
                                           (%neutralized-attribute-p (car pair) (cdr pair)))
                                         (%attribute-pairs element)))))
              ;; Comment data is written raw: a frozen file keeps none.
              :keep-comments nil))

;;; Document skeleton shared by PAGE and FREEZE.

(defun %element (name attributes &rest children)
  "A head element built directly (EL refuses <script>; these are our own)."
  (stack-html:make-html-element name :attributes attributes :children children))

(defun %style-element ()
  (let ((css (canvas-stylesheet)))
    ;; <style> text is written raw by html-protocol: refuse a theme that would close it.
    (when (search "</style" css :test #'char-equal)
      (%canvas-error "theme stylesheet ~S contains \"</style\"" *canvas-theme*))
    (%element "style" nil (%text css))))

(defun %document (lang head body &key body-attributes)
  (unless (stringp lang)
    (%canvas-error "lang must be a string: ~S" lang))
  (stack-html:make-html-document
   :children (list (make-instance 'stack-html:html-doctype)
                   (%element "html" (list (cons "lang" lang))
                             (stack-html:make-html-element "head" :children head)
                             (stack-html:make-html-element "body" :attributes body-attributes
                                                                  :children body)))))

(defun %head-start ()
  (list (%element "meta" '(("charset" . "utf-8")))
        (%element "meta" '(("name" . "viewport")
                           ("content" . "width=device-width, initial-scale=1")))))

(defun page (body &key (title "canvas") (lang "en") (stylesheet :link) (htmx t))
  (let ((resolver (%resolver)))
    (unless (member stylesheet '(:link :inline nil))
      (%canvas-error "stylesheet must be :link, :inline or NIL, not ~S" stylesheet))
    (let* ((nodes (%nodes (render body)))
           (violations (csp-violations nodes)))
      (when violations
        (restart-case (error 'canvas-csp-error
                             :violations violations
                             :message (format nil "~D CSP violation~:P in the page body"
                                              (length violations)))
          (strip-violations ()
            :report "Remove the offending elements and attributes and go on."
            (setf nodes (%strip-violations nodes)))))
      (flet ((asset-element (name make)
               (let ((url (resolve-asset-url resolver name)))
                 (when url (list (funcall make url))))))
        (%document
         lang
         (append (%head-start)
                 (when htmx
                   (list (%element "meta" (list (cons "name" "htmx-config")
                                                (cons "content" (stack-json:encode +htmx-config+))))))
                 (list (el :title nil title))
                 (when (eq stylesheet :link)
                   (asset-element "canvas.css"
                                  (lambda (url)
                                    (%element "link" (list (cons "rel" "stylesheet")
                                                           (cons "href" url))))))
                 (when (eq stylesheet :inline)
                   (list (%style-element)))
                 (when htmx
                   (loop for name in '("htmx.min.js" "htmx-ext-sse.min.js")
                         append (asset-element name
                                               (lambda (url)
                                                 (%element "script" (list (cons "src" url))))))))
         nodes)))))

(defun freeze (fn &key args title)
  (unless (or (functionp fn)
              (and fn (symbolp fn) (fboundp fn)
                   (not (macro-function fn)) (not (special-operator-p fn))))
    (%canvas-error "freeze needs a function or an fbound function name: ~S" fn))
  (unless (%plist-p args)
    (%canvas-error "freeze :args must be a plist: ~S" args))
  (let ((title (or title (if (symbolp fn) (string-downcase (symbol-name fn)) "canvas")))
        (*canvas-resolver* (make-freeze-resolver)))
    (to-html (%document "en"
                        (append (%head-start)
                                (list (el :title nil title) (%style-element)))
                        (%nodes (neutralize (apply fn args)))
                        :body-attributes '(("data-canvas-frozen" . ""))))))
