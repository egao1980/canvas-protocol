(in-package #:canvas-protocol/tests)

;;; Fake resolver: /t/<PACKAGE>::<NAME>?k=v&k=v (no percent-encoding), assets /a/<name>.

(defclass fake-resolver (canvas-resolver)
  ((calls :initform nil :accessor fake-calls)))

(defmethod resolve-canvas-url ((r fake-resolver) symbol params)
  (push (list symbol params) (fake-calls r))
  (format nil "/t/~A::~A~@[?~{~A~^&~}~]"
          (package-name (symbol-package symbol)) (symbol-name symbol)
          (and params (mapcar (lambda (p) (format nil "~A=~A" (car p) (cdr p))) params))))

(defmethod resolve-asset-url ((r fake-resolver) name)
  (format nil "/a/~A" name))

(defmacro with-fake ((&optional (var (gensym "R"))) &body body)
  `(let* ((,var (make-instance 'fake-resolver))
          (*canvas-resolver* ,var))
     (declare (ignorable ,var))
     ,@body))

(defun html (object)
  "TO-HTML under a fresh fake resolver."
  (with-fake () (to-html object)))

(defun s+ (&rest strings)
  (apply #'concatenate 'string strings))

(defun contains (needle haystack)
  (and (search needle haystack) t))

(defparameter +u+ "/t/CANVAS-PROTOCOL/TESTS::"
  "URL prefix the fake resolver gives canvases in this package.")

;;; Raw html-protocol nodes (attributes are an alist; html-protocol 0.1.1 prints it in list order).

(defun raw (tag attrs &rest children)
  (stack-html:make-html-element
   tag :attributes attrs
       :children (mapcar (lambda (c)
                           (if (stringp c) (make-instance 'stack-html:html-text :data c) c))
                         children)))

(defun walk-elements (tree fn)
  "Call FN on every html-element in TREE (a node, a list of nodes, or a document)."
  (labels ((walk (n)
             (cond ((listp n) (mapc #'walk n))
                   ((stack-html:html-document-p n)
                    (mapc #'walk (stack-html:html-document-children n)))
                   ((stack-html:html-element-p n)
                    (funcall fn n)
                    (mapc #'walk (stack-html:html-element-children n))))))
    (walk tree)))

(defun attribute-names (tree)
  (let ((names '()))
    (walk-elements tree (lambda (e)
                          (dolist (a (stack-html:html-element-attributes e))
                            (push (string-downcase (car a)) names))))
    (nreverse names)))

(defun element-names (tree)
  (let ((names '()))
    (walk-elements tree (lambda (e) (push (stack-html:html-element-name e) names)))
    (nreverse names)))

(defun octets-hex (octets)
  (format nil "~(~{~2,'0X~}~)" (coerce octets 'list)))

(defun sha256-hex (octets)
  (octets-hex (stack-crypto:digest octets :algorithm :sha256
                                          :backend (crypto-backend-ironclad:make-ironclad-crypto-backend))))

;;; Canvas functions used by links in the tests.

(defun tab-demo (&key tab) tab)
(defun people (&key sort page q) (list sort page q))
(defun inc (&key n) n)
(defun save (&rest params) params)
(defun ticks () nil)
(defun |weird name| () nil)

(defparameter *rows*
  (list (list :name "Bo" :age 30)
        (list :name "Al" :age 25)
        (list :name "<i>Cy</i>" :age 41)))

(defun cols ()
  (list (column "Name" :name :name "name" :sortable t)
        (column "Age" (lambda (r) (getf r :age)) :name "age" :sortable t :align :end)))

(defun coercion-error (value type &rest keys)
  (handler-case (apply #'coerce-param value type keys)
    (canvas-coercion-error (c) c)))

;;; Every component, for the CSP and freeze scans.
(defun gallery (&key sort page)
  (let ((err (coercion-error "abc" :integer :field "age")))
    (stack
     (list
      (card (list (kv-list '(("Name" . "Ann") ("Age" . 3)))
                  (badge "ok" :status :ok)
                  (code-block (format nil "(defun f ()~%  \"<x>\")") :language "lisp"))
            :title "Card <b>" :footer "foot")
      (grid (list (el :p nil "1") (el :p nil "2") (el :p nil "3")) :columns 3 :gap :s)
      (stack (list (el :span nil "a") (el :span nil "b")) :direction :horizontal)
      (details "More" (el :p nil "hidden") :open t)
      (tabs '(("a" . "Alpha") ("b" . "Beta")) :id "tabs" :active "a"
            :link (canvas-link 'tab-demo) :content (el :p nil "alpha"))
      (table *rows* (cols) :id "people" :link (canvas-link 'people) :sort sort :page page
                           :page-size 2 :caption "People")
      (table nil (cols))
      (form (list (fieldset (list (text-input "name" :label "Name" :value "A&B" :required t)
                                  (number-input "n" :value 3 :min 0 :max 10 :step 1)
                                  (date-input "d" :value '(2026 11 22))
                                  (checkbox "agree" :label "I agree" :checked t)
                                  (select-input "c" '(("r" . "Red") ("g" . "Green")) :value "g")
                                  (textarea "t" :value "</textarea>" :rows 3)
                                  (text-input "age" :label "Age" :value "abc" :error err))
                            :legend "Who")
                  (submit-button "Save"))
            :link (canvas-link 'save) :id "f")
      (live-region (canvas-link 'ticks) "tick" :id "clock" :content "waiting")
      (el :button (hx-post (canvas-link 'inc :n 1) :target "#c" :swap :outer-html
                           :vals '(:a "1"))
          "Inc")))))
