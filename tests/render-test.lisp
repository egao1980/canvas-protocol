(in-package #:canvas-protocol/tests)

(deftest el-basic
  (ok (stack-html:html-element-p (el :div nil)))
  (ok (string= "<div class=\"x\" id=\"a\">hi</div>"
               (html (el :div '(:class "x" :id "a") "hi"))))
  (ok (string= "<p>x</p>" (html (el "P" nil "x")))))

(deftest el-attribute-values
  (ok (string= "<input type=\"checkbox\" checked=\"\" tabindex=\"2\">"
               (html (el :input '(:type "checkbox" :checked t :disabled nil :tabindex 2))))))

(deftest el-children
  (ok (string= "<p>a<b>b</b>3</p>" (html (el :p nil "a" (el :b nil "b") 3))))
  (ok (string= "<ul><li>1</li><li>2</li></ul>"
               (html (el :ul nil (list (el :li nil "1") nil (list (el :li nil "2")))))))
  (ok (string= "<div><hr></div>" (html (el :div nil (stack-html:make-html-element "hr"))))))

(deftest el-bad-names
  (ok (signals (el "di v" nil) 'canvas-error))
  (ok (signals (el "" nil) 'canvas-error))
  (ok (signals (el :div '(:|a"b| "x")) 'canvas-error))
  (ok (signals (el :div '(:class)) 'canvas-error))
  (ok (signals (el :div '(:title 1.5)) 'canvas-error)))

(deftest el-refuses-csp-hazards
  (dolist (tag '(:script :iframe :object :embed "SCRIPT"))
    (ok (signals (el tag nil) 'canvas-csp-error) (format nil "tag ~A" tag)))
  (dolist (attrs '((:onclick "x()") (:|ONLOAD| "x()") (:|hx-on:click| "x()")
                   (:|hx-on--click| "x()") (:|data-hx-on:click| "x()")
                   (:style "color:red") (:hx-vars "a:1") (:hx-vals "js:{a:1}")
                   (:hx-trigger "click[ctrlKey]")
                   (:href "javascript:alert(1)") (:href " JavaScript:alert(1)")
                   (:hx-get "javascript:x") (:src "JAVASCRIPT:x")))
    (ok (signals (el :button attrs "b") 'canvas-csp-error) (format nil "attrs ~S" attrs)))
  (ok (typep (handler-case (el :a '(:onclick "x")) (canvas-csp-error (c) c)) 'canvas-error)))

(deftest escaping-hostile-text
  (ok (string= "<p>&lt;script&gt;alert(1)&lt;/script&gt;&amp;amp;</p>"
               (html (el :p nil "<script>alert(1)</script>&amp;"))))
  (ok (string= "&lt;b&gt;x&lt;/b&gt;" (html "<b>x</b>")))
  (ok (string= "a&lt;b&lt;i&gt;" (html (list "a<b" (list "<i>")))))
  (ok (string= "<p title=\"&quot;>&lt;script>\"></p>"
               (html (el :p '(:title "\"><script>"))))))

(defclass user-thing () ((label :initarg :label :reader user-thing-label)))
(defmethod render ((object user-thing))
  (el :mark nil (user-thing-label object)))

(deftest render-methods
  (let ((node (stack-html:make-html-element "hr")))
    (ok (eq node (render node))))
  (ok (null (render nil)))
  (ok (string= "42" (html 42)))
  (ok (string= "1/2" (html 1/2)))
  (ok (string= "ab" (html (list "a" nil "b"))))
  (ok (string= "<p><mark>&lt;u&gt;</mark></p>"
               (html (el :p nil (make-instance 'user-thing :label "<u>")))))
  (ok (signals (render (make-hash-table)) 'canvas-error))
  (ok (signals (html (el :p nil (make-hash-table))) 'canvas-error)))

(deftest to-html-documents
  (let ((doc (stack-html:make-html-document
              :children (list (make-instance 'stack-html:html-doctype)
                              (stack-html:make-html-element
                               "html" :children (list (stack-html:make-html-element
                                                       "body" :children (list (el :p nil "x")))))))))
    (ok (string= "<!DOCTYPE html><html><body><p>x</p></body></html>" (to-html doc))))
  (ok (string= "" (to-html nil))))
