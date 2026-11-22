(in-package #:canvas-protocol/tests)

(defun kinds (tree) (mapcar (lambda (v) (getf v :kind)) (csp-violations tree)))

(deftest csp-violation-kinds
  (ok (equal '(:inline-script) (kinds (raw "script" nil "alert(1)"))))
  (ok (equal '(:inline-script) (kinds (raw "script" nil))) "no src")
  (ok (null (kinds (raw "script" '(("src" . "/a/htmx.min.js"))))))
  (ok (equal '(:inline-script) (kinds (raw "script" '(("src" . "/x.js")) "more"))))
  (ok (equal '(:embed :embed :embed) (kinds (list (raw "iframe" nil) (raw "object" nil) (raw "embed" nil)))))
  (ok (equal '(:event-handler) (kinds (raw "img" '(("onerror" . "x()"))))))
  (ok (equal '(:event-handler) (kinds (raw "body" '(("ONLOAD" . "x()"))))))
  (ok (equal '(:hx-on :hx-on) (kinds (list (raw "b" '(("hx-on:click" . "x()")))
                                           (raw "b" '(("data-hx-on:click" . "x()")))))))
  (ok (equal '(:javascript-url :javascript-url)
             (kinds (list (raw "a" '(("href" . " javascript:x()"))) (raw "form" '(("action" . "JavaScript:x")))))))
  (ok (equal '(:inline-style) (kinds (raw "p" '(("style" . "color:red"))))))
  (ok (equal '(:eval-trigger) (kinds (raw "p" '(("hx-trigger" . "keyup[key=='Enter']"))))))
  (ok (equal '(:js-vals :js-vals) (kinds (list (raw "p" '(("hx-vals" . "js:{a:b()}")))
                                               (raw "p" '(("hx-vars" . "a:b()")))))))
  (ok (null (kinds (raw "p" '(("hx-vals" . "{\"a\":\"1\"}") ("hx-trigger" . "click")))))))

(deftest csp-violation-shape-and-order
  (let ((v (csp-violations (raw "div" nil (raw "a" '(("onclick" . "x"))) (raw "script" nil "y")))))
    (ok (= 2 (length v)))
    (ok (equal '(:kind :event-handler :element "a" :attribute "onclick") (first v)))
    (ok (equal '(:kind :inline-script :element "script" :attribute nil) (second v)))))

(deftest csp-scans-documents-and-components
  (ok (null (csp-violations (stack-html:make-html-document :children (list (raw "p" nil "x"))))))
  (ok (equal '(:inline-script)
             (kinds (stack-html:make-html-document :children (list (raw "script" nil "x"))))))
  (ok (equal '(:inline-script) (with-fake () (kinds (card (raw "script" nil "x")))))
      "components are rendered first"))

(deftest csp-gallery-clean
  (let* ((doc (with-fake () (page (gallery :sort "-age" :page "1"))))
         (names (attribute-names doc)))
    (ok (null (csp-violations doc)))
    (ok (notany (lambda (n) (and (>= (length n) 2) (string= "on" n :end2 2))) names) "no on*")
    (ok (notany (lambda (n) (search "hx-on" n)) names))
    (ok (not (member "style" names :test #'string=)) "no style attributes")
    (ok (not (member "class" names :test #'string=)) "classless")
    (walk-elements doc (lambda (e)
                         (when (string= "script" (stack-html:html-element-name e))
                           (ok (stack-html:element-attr e "src"))
                           (ok (null (stack-html:html-element-children e))))))
    (ok (= 2 (count "script" (element-names doc) :test #'string=)))))

(deftest csp-data-vocabulary
  (let* ((doc (with-fake () (page (gallery))))
         (data (remove-if-not (lambda (n) (and (> (length n) 5) (string= "data-" n :end2 5)))
                              (attribute-names doc))))
    (ok (subsetp data '("data-layout" "data-gap" "data-columns" "data-align" "data-badge"
                        "data-language" "data-chart" "data-canvas-frozen")
                 :test #'string=)
        (format nil "~S" (remove-duplicates data :test #'string=)))))
