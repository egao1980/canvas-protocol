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
