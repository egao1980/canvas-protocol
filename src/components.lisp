(in-package #:canvas-protocol)

;;; Layout components. Classless semantic HTML: no class attributes, only the
;;; closed data-* vocabulary of the brief. Attribute order: id, own attributes,
;;; then component-attrs. CHILDREN is anything RENDER accepts (a list is a list
;;; of children). Validation happens in the constructor (VALIDATE-COMPONENT
;;; methods) and signals CANVAS-ERROR.
;;;
;;; stack     <div data-layout="stack|row" [data-gap="none|s|l"]>children</div>
;;;           gap :none :s :m :l (default :m, omitted); direction :vertical (stack)
;;;           or :horizontal (row)
;;; grid      <div data-layout="grid" data-columns="N" [data-gap]>; columns integer 1..6
;;; card      <article>[<header><h3>title</h3></header>]children[<footer>footer</footer>]</article>
;;; details   <details [open=""]><summary>summary</summary>children</details>
;;; fieldset  <fieldset>[<legend>legend</legend>]children</fieldset>
;;; tabs      id required. TABS: strings or (key . label) conses; ACTIVE a key
;;;           (default the first; unknown -> CANVAS-ERROR).
;;;           <section id="ID"><nav role="tablist">
;;;             <button type="button" role="tab" aria-selected="true|false"
;;;                     [hx-get="<link + tab=KEY>" hx-target="#ID" hx-swap="outerHTML"]>label</button>...
;;;           </nav><div role="tabpanel">content</div></section>
;;; code-block <pre><code [data-language="L"]>text</code></pre>; language [a-z0-9+#-]+
;;; kv-list   <dl><dt>k</dt><dd>v</dd>...</dl>; PAIRS an alist, keys and values renderable
;;; badge     <span data-badge="STATUS">text</span>; status :neutral (default) :info :ok :warn :error
;;; live-region <div [id] hx-ext="sse" sse-connect="URL" sse-swap="EVENT" [hx-swap="S"]
;;;              aria-live="polite">content</div>; link resolving to NIL -> no hx-ext/sse-* attributes

(defclass container-component (component)
  ((children :initarg :children :initform nil :reader component-children)))

(defclass stack (container-component)
  ((gap :initarg :gap :initform :m :reader layout-gap)
   (direction :initarg :direction :initform :vertical :reader stack-direction)))

(defun stack (children &key (gap :m) (direction :vertical) id attrs)
  (%make 'stack :children children :gap gap :direction direction :id id :attrs attrs))

(defclass grid (container-component)
  ((columns :initarg :columns :initform 2 :reader grid-columns)
   (gap :initarg :gap :initform :m :reader layout-gap)))

(defun grid (children &key (columns 2) (gap :m) id attrs)
  (%make 'grid :children children :columns columns :gap gap :id id :attrs attrs))

(defclass card (container-component)
  ((title :initarg :title :initform nil :reader card-title)
   (footer :initarg :footer :initform nil :reader card-footer)))

(defun card (children &key title footer id attrs)
  (%make 'card :children children :title title :footer footer :id id :attrs attrs))

(defclass details (container-component)
  ((summary :initarg :summary :reader details-summary)
   (open :initarg :open :initform nil :reader details-open-p)))

(defun details (summary children &key open id attrs)
  (%make 'details :summary summary :children children :open open :id id :attrs attrs))

(defclass fieldset (container-component)
  ((legend :initarg :legend :initform nil :reader fieldset-legend)))

(defun fieldset (children &key legend id attrs)
  (%make 'fieldset :children children :legend legend :id id :attrs attrs))

(defclass tabs (component)
  ((tabs :initarg :tabs :reader tabs-tabs)
   (active :initarg :active :initform nil :reader tabs-active)
   (link :initarg :link :initform nil :reader tabs-link)
   (content :initarg :content :initform nil :reader tabs-content)))

(defun tabs (tabs &key id active link content attrs)
  (%make 'tabs :tabs tabs :id id :active active :link link :content content :attrs attrs))

(defclass code-block (component)
  ((text :initarg :text :reader code-block-text)
   (language :initarg :language :initform nil :reader code-block-language)))

(defun code-block (text &key language id attrs)
  (check-type text string)
  (%make 'code-block :text text :language language :id id :attrs attrs))

(defclass kv-list (component)
  ((pairs :initarg :pairs :reader kv-list-pairs)))

(defun kv-list (pairs &key id attrs)
  (check-type pairs list)
  (%make 'kv-list :pairs pairs :id id :attrs attrs))

(defclass badge (component)
  ((text :initarg :text :reader badge-text)
   (status :initarg :status :initform :neutral :reader badge-status)))

(defun badge (text &key (status :neutral) id attrs)
  (%make 'badge :text text :status status :id id :attrs attrs))

(defclass live-region (component)
  ((link :initarg :link :reader live-region-link)
   (event :initarg :event :reader live-region-event)
   (content :initarg :content :initform nil :reader live-region-content)
   (swap :initarg :swap :initform nil :reader live-region-swap)))

(defun live-region (link event &key id content swap attrs)
  (%make 'live-region :link link :event event :id id :content content :swap swap :attrs attrs))

;;; Validation

(defun %gap-attr (gap)
  (if (eq gap :m) nil (string-downcase (symbol-name gap))))

(defun %check-gap (gap)
  (unless (member gap '(:none :s :m :l))
    (%canvas-error "gap must be one of :none :s :m :l, not ~S" gap)))

(defmethod validate-component ((c stack))
  (%check-gap (layout-gap c))
  (unless (member (stack-direction c) '(:vertical :horizontal))
    (%canvas-error "direction must be :vertical or :horizontal, not ~S" (stack-direction c)))
  (call-next-method))

(defmethod validate-component ((c grid))
  (unless (typep (grid-columns c) '(integer 1 6))
    (%canvas-error "grid columns must be an integer 1..6, not ~S" (grid-columns c)))
  (%check-gap (layout-gap c))
  (call-next-method))

(defun %tab-key (tab) (if (consp tab) (car tab) tab))
(defun %tab-label (tab) (if (consp tab) (cdr tab) tab))

(defmethod validate-component ((c tabs))
  (let ((tabs (tabs-tabs c))
        (active (tabs-active c)))
    (unless (stringp (component-id c))
      (%canvas-error "tabs need an :id (the hx-target of the tab buttons)"))
    (unless (and (consp tabs) (%proper-list-p tabs))
      (%canvas-error "tabs must be a non-empty list: ~S" tabs))
    (dolist (tab tabs)
      (unless (stringp (%tab-key tab))
        (%canvas-error "a tab is a string or a (key . label) cons with a string key: ~S" tab)))
    (let ((keys (mapcar #'%tab-key tabs)))
      (unless (= (length keys) (length (remove-duplicates keys :test #'string=)))
        (%canvas-error "duplicate tab keys in ~S" keys))
      (unless (or (null active) (member active keys :test #'equal))
        (%canvas-error "active tab ~S is not one of ~S" active keys)))
    (unless (or (null (tabs-link c)) (canvas-link-p (tabs-link c)))
      (%canvas-error "tabs :link must be a canvas-link: ~S" (tabs-link c))))
  (call-next-method))

(defmethod validate-component ((c code-block))
  (let ((language (code-block-language c)))
    (unless (or (null language)
                (%token-p language #'%code-language-char-p #'%code-language-char-p))
      (%canvas-error "code-block language must match [a-z0-9+#-]+, not ~S" language)))
  (call-next-method))

(defun %code-language-char-p (c)
  (or (%ascii-lower-p c) (%ascii-digit-p c) (find c "+#-")))

(defmethod validate-component ((c kv-list))
  (unless (every #'consp (kv-list-pairs c))
    (%canvas-error "kv-list pairs must be an alist: ~S" (kv-list-pairs c)))
  (call-next-method))

(defparameter +badge-statuses+ '(:neutral :info :ok :warn :error))

(defmethod validate-component ((c badge))
  (unless (member (badge-status c) +badge-statuses+)
    (%canvas-error "badge status must be one of ~S, not ~S" +badge-statuses+ (badge-status c)))
  (call-next-method))

(defmethod validate-component ((c live-region))
  (unless (canvas-link-p (live-region-link c))
    (%canvas-error "live-region link must be a canvas-link: ~S" (live-region-link c)))
  (%event-name (live-region-event c))
  (when (live-region-swap c) (hx-swap-value (live-region-swap c)))
  (call-next-method))

;;; Rendering

(defmethod render ((c stack))
  (el :div (%own c :data-layout (if (eq (stack-direction c) :horizontal) "row" "stack")
                   :data-gap (%gap-attr (layout-gap c)))
      (component-children c)))

(defmethod render ((c grid))
  (el :div (%own c :data-layout "grid" :data-columns (grid-columns c)
                   :data-gap (%gap-attr (layout-gap c)))
      (component-children c)))

(defmethod render ((c card))
  (el :article (%own c)
      (when (card-title c) (el :header nil (el :h3 nil (card-title c))))
      (component-children c)
      (when (card-footer c) (el :footer nil (card-footer c)))))

(defmethod render ((c details))
  (el :details (%own c :open (and (details-open-p c) t))
      (el :summary nil (details-summary c))
      (component-children c)))

(defmethod render ((c fieldset))
  (el :fieldset (%own c)
      (when (fieldset-legend c) (el :legend nil (fieldset-legend c)))
      (component-children c)))

(defmethod render ((c tabs))
  (let* ((tabs (tabs-tabs c))
         (active (or (tabs-active c) (%tab-key (first tabs))))
         (link (tabs-link c))
         (target (%str "#" (component-id c))))
    (el :section (%own c)
        (el :nav '(:role "tablist")
            (loop for tab in tabs
                  for key = (%tab-key tab)
                  collect (el :button
                              (append (list :type "button" :role "tab"
                                            :aria-selected (if (equal key active) "true" "false"))
                                      (when link
                                        (hx-get (%link-merge link :tab key)
                                                :target target :swap :outer-html)))
                              (%tab-label tab))))
        (el :div '(:role "tabpanel") (tabs-content c)))))

(defmethod render ((c code-block))
  (el :pre (%own c)
      (el :code (list :data-language (code-block-language c)) (code-block-text c))))

(defmethod render ((c kv-list))
  (el :dl (%own c)
      (loop for (key . value) in (kv-list-pairs c)
            collect (el :dt nil key)
            collect (el :dd nil value))))

(defmethod render ((c badge))
  (el :span (%own c :data-badge (string-downcase (symbol-name (badge-status c))))
      (badge-text c)))

(defmethod render ((c live-region))
  (let* ((connect (sse-connect (live-region-link c)))
         (sse (when connect
                (append connect (sse-swap (live-region-event c) :swap (live-region-swap c))))))
    (el :div (apply #'%own c (append sse (list :aria-live "polite")))
        (live-region-content c))))
