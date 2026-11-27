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
;;; 0.2.0 (HX2): PAGE &key (keys nil). KEYS T adds
;;;   <script src="(resolve-asset-url r "canvas-keys.js")"></script>
;;; right after the SSE extension's script (so the head ends with the three
;;; scripts htmx, htmx-ext-sse, canvas-keys). KEYS T with HTMX NIL ->
;;; CANVAS-ERROR (the bindings only make htmx requests); KEYS other than T or
;;; NIL -> CANVAS-ERROR. KEYS NIL leaves the document exactly as in 0.1.
;;; Decision: opt-in, not detected from BODY, because key-bound elements often
;;; arrive later in swapped fragments; an element bound without the script
;;; sends nothing (no "canvas:key" event), so forgetting it fails safe.
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
;;; 0.2.0 (HX2): also without data-canvas-keys attributes (a frozen file loads
;;; no script, so the bindings are dead weight).
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
;;;
;;; Scripts (0.3.0, canvas-protocol#15). PAGE &key (scripts nil): a list of
;;; entries, each an asset name (ASSET-NAMES: built-in or REGISTER-ASSET) or
;;; (name &key module defer). Each becomes
;;;   <script src="(resolve-asset-url r name)"[ type="module"][ defer=""]></script>
;;; at the end of <head>, after the built-in scripts (htmx, htmx-ext-sse,
;;; canvas-keys), in the given order; written with :htmx nil too (an app
;;; script need not use htmx). A resolver giving NIL for a name skips it, as
;;; for the built-ins. An unknown name -> CANVAS-ASSET-NOT-FOUND; an asset
;;; whose content type is not text/javascript or application/javascript (a
;;; .wasm is fetched by its loader script, never a <script>), a name loaded
;;; twice (built-ins included), a malformed entry or an unknown option ->
;;; CANVAS-ERROR. All checked before the body is rendered. :scripts nil leaves
;;; the document byte-identical to 0.2. Every script is same-origin with src
;;; and no text, so the CSP stays script-src 'self' (a WASM toolkit also needs
;;; 'wasm-unsafe-eval' in the server's policy to compile its module).
;;;
;;; FREEZE &key (scripts nil): decision: opt-in. The same entries, checked the
;;; same way, become <script src="NAME"...></script> after the <style>, NAME
;;; as a relative URL: for a frozen page served as a static file next to its
;;; assets (copy each ASSET-BYTES to that relative path). The body is still
;;; NEUTRALIZEd (no htmx, no URLs, no data-canvas-keys); the scripts work on
;;; the static DOM. :scripts nil -> byte-identical to 0.2: frozen pages run no
;;; script unless asked.

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
                        ;; MAKE-HTML-ELEMENT canonicalises the name (SVG camelCase kept).
                        (stack-html:make-html-element
                         (stack-html:html-element-name node)
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
  "ELEMENT's attributes as fresh (name . string) pairs, NAME as
STACK-HTML:CANONICAL-ATTRIBUTE-NAME: lowercased, except SVG camelCase names
(viewBox, preserveAspectRatio, ...), which keep their case. Every CSP and URL
check folds case itself."
  (loop for (name . value) in (stack-html:html-element-attributes element)
        collect (cons (stack-html:canonical-attribute-name name)
                      (if (stringp value) value (princ-to-string value)))))

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
  (let ((name (string-downcase name)))
    (or (%prefix-p "hx-" name)
        (%prefix-p "data-hx-" name)
        (%prefix-p "sse-" name)
        (%prefix-p "data-sse-" name)
        (string= name "data-canvas-keys")
        (%attribute-violation name value)
        (and (member name +neutralized-url-attributes+ :test #'string=)
             (not (%offline-url-p value))))))

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

(defun %keys-script (keys htmx)
  "PAGE's KEYS option -> the list of extra asset names after the SSE script.
See the file header."
  (unless (eq keys t)
    (%canvas-error "page :keys must be T or NIL, not ~S" keys))
  (unless htmx
    (%canvas-error "page :keys t needs :htmx t (the key bindings only send htmx requests)"))
  (list "canvas-keys.js"))

(defun %script-entry (entry)
  "One :scripts entry -> (name module defer). See the file header (Scripts)."
  (multiple-value-bind (name options)
      (cond ((stringp entry) (values entry nil))
            ((and (consp entry) (stringp (car entry)) (%plist-p (cdr entry)))
             (values (car entry) (cdr entry)))
            (t (%canvas-error ":scripts entry must be an asset name or (name &key module defer), not ~S"
                              entry)))
    (loop for (key) on options by #'cddr
          unless (member key '(:module :defer))
            do (%canvas-error ":scripts entry ~S: unknown option ~S (:module, :defer)" entry key))
    (unless (%asset-entry name)
      (error 'canvas-asset-not-found :name name))
    (let ((type (asset-content-type name)))
      (unless (or (%prefix-p "text/javascript" type) (%prefix-p "application/javascript" type))
        (%canvas-error ":scripts asset ~S is ~A, not JavaScript" name type)))
    (list name (and (getf options :module) t) (and (getf options :defer) t))))

(defun %script-entries (scripts loaded)
  "PAGE / FREEZE :scripts -> a list of (name module defer). LOADED: the names
the head already loads (a repeat -> CANVAS-ERROR)."
  (unless (and (listp scripts) (null (cdr (last scripts))))
    (%canvas-error ":scripts must be a list of asset names, not ~S" scripts))
  (let ((entries (mapcar #'%script-entry scripts)))
    (loop for (entry . rest) on entries
          for name = (first entry)
          when (or (member name loaded :test #'string=)
                   (member name rest :key #'first :test #'string=))
            do (%canvas-error ":scripts loads ~S twice" name))
    entries))

(defun %script-element (url module defer)
  (%element "script" (append (list (cons "src" url))
                             (when module (list (cons "type" "module")))
                             (when defer (list (cons "defer" ""))))))

(defun page (body &key (title "canvas") (lang "en") (stylesheet :link) (htmx t) keys scripts)
  (let* ((resolver (%resolver))
         (extra-scripts (and keys (%keys-script keys htmx)))
         (app-scripts (and scripts
                           (%script-entries scripts
                                            (and htmx (append '("htmx.min.js" "htmx-ext-sse.min.js")
                                                              extra-scripts))))))
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
                   (loop for name in (append '("htmx.min.js" "htmx-ext-sse.min.js")
                                             extra-scripts)
                         append (asset-element name
                                               (lambda (url)
                                                 (%element "script" (list (cons "src" url)))))))
                 (loop for (name module defer) in app-scripts
                       append (asset-element name
                                             (lambda (url) (%script-element url module defer)))))
         nodes)))))

(defun freeze (fn &key args title scripts)
  (unless (or (functionp fn)
              (and fn (symbolp fn) (fboundp fn)
                   (not (macro-function fn)) (not (special-operator-p fn))))
    (%canvas-error "freeze needs a function or an fbound function name: ~S" fn))
  (unless (%plist-p args)
    (%canvas-error "freeze :args must be a plist: ~S" args))
  (let ((title (or title (if (symbolp fn) (string-downcase (symbol-name fn)) "canvas")))
        (app-scripts (and scripts (%script-entries scripts nil)))
        (*canvas-resolver* (make-freeze-resolver)))
    (to-html (%document "en"
                        (append (%head-start)
                                (list (el :title nil title) (%style-element))
                                ;; Relative src = the asset name (FREEZE :scripts, file header).
                                (loop for (name module defer) in app-scripts
                                      collect (%script-element name module defer)))
                        (%nodes (neutralize (apply fn args)))
                        :body-attributes '(("data-canvas-frozen" . ""))))))
