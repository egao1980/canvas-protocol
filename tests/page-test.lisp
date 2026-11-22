(in-package #:canvas-protocol/tests)

(defparameter +head-start+
  "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")

(defparameter +htmx-config-meta+
  (s+ "<meta name=\"htmx-config\" content=\"{&quot;allowEval&quot;:false,&quot;allowScriptTags&quot;:false,"
      "&quot;historyCacheSize&quot;:0,&quot;includeIndicatorStyles&quot;:false,"
      "&quot;refreshOnHistoryMiss&quot;:true,&quot;selfRequestsOnly&quot;:true}\">"))

(deftest page-golden
  (ok (string= (s+ +head-start+ +htmx-config-meta+
                   "<title>T&amp;</title><link rel=\"stylesheet\" href=\"/a/canvas.css\">"
                   "<script src=\"/a/htmx.min.js\"></script><script src=\"/a/htmx-ext-sse.min.js\"></script>"
                   "</head><body><p>hi</p></body></html>")
               (with-fake () (to-html (page (el :p nil "hi") :title "T&")))))
  (ok (stack-html:html-document-p (with-fake () (page "x")))))

(deftest page-options
  (ok (string= (s+ +head-start+ "<title>canvas</title><link rel=\"stylesheet\" href=\"/a/canvas.css\">"
                   "</head><body><p>hi</p></body></html>")
               (with-fake () (to-html (page (el :p nil "hi") :htmx nil)))))
  (let ((out (with-fake () (to-html (page "x" :stylesheet :inline :lang "de")))))
    (ok (contains "<html lang=\"de\">" out))
    (ok (contains (s+ "<style>" (canvas-stylesheet) "</style>") out))
    (ok (not (contains "<link" out))))
  (let ((out (with-fake () (to-html (page "x" :stylesheet nil :htmx nil)))))
    (ok (not (contains "<link" out)))
    (ok (not (contains "<style" out)))
    (ok (not (contains "<script" out)))))

(deftest page-needs-resolver
  (let ((*canvas-resolver* nil))
    (ok (signals (page "x") 'canvas-no-resolver))))

(deftest page-refuses-violations
  (let ((body (list (el :p nil "ok") (raw "script" nil "alert(1)")
                    (raw "button" '(("onclick" . "steal()")) "b"))))
    (let ((c (handler-case (with-fake () (page body)) (canvas-csp-error (c) c))))
      (ok (typep c 'canvas-csp-error))
      (ok (equal '(:inline-script :event-handler)
                 (mapcar (lambda (v) (getf v :kind)) (csp-error-violations c)))))
    (let ((out (with-fake ()
                 (handler-bind ((canvas-csp-error (lambda (c) (declare (ignore c))
                                                    (invoke-restart 'strip-violations))))
                   (to-html (page body))))))
      (ok (not (contains "alert(1)" out)))
      (ok (not (contains "onclick" out)))
      (ok (contains "<button>b</button>" out))
      (ok (contains "<script src=\"/a/htmx.min.js\"></script>" out)))))

(deftest htmx-config-constant
  (ok (equal '("allowEval" "allowScriptTags" "historyCacheSize" "includeIndicatorStyles"
               "refreshOnHistoryMiss" "selfRequestsOnly")
             (mapcar #'car +htmx-config+)))
  (ok (eq :false (cdr (assoc "allowEval" +htmx-config+ :test #'string=)))))
