(in-package #:canvas-protocol/tests)

;;; Links and resolvers

(deftest canvas-link-normalizes
  (let ((l (canvas-link 'inc :n 1 :flag t :skip nil :mode :fast :s "a b")))
    (ok (canvas-link-p l))
    (ok (eq 'inc (canvas-link-function l)))
    (ok (equal '(("n" . "1") ("flag" . "true") ("mode" . "fast") ("s" . "a b"))
               (canvas-link-params l))))
  (ok (equal '(("n" . "2") ("m" . "3")) (canvas-link-params (canvas-link 'inc :n 1 :m 3 :n 2))))
  (ok (null (canvas-link-params (canvas-link 'inc)))))

(deftest canvas-link-refuses
  (ok (signals (canvas-link 'no-such-canvas-function-xyz) 'canvas-error))
  (ok (signals (canvas-link :inc) 'canvas-error))
  (ok (signals (canvas-link (lambda () nil)) 'canvas-error))
  (ok (signals (canvas-link "inc") 'canvas-error))
  (ok (signals (canvas-link 'inc :n 1.5) 'canvas-error))
  (ok (signals (canvas-link 'inc :n) 'canvas-error))
  (ok (signals (canvas-link 'inc :n (list 1)) 'canvas-error)))

(deftest canvas-url-through-fake
  (with-fake (r)
    (ok (string= (s+ +u+ "INC?n=1") (canvas-url 'inc :n 1)))
    (ok (equal (list (list 'inc '(("n" . "1")))) (fake-calls r)) "resolver got symbol + alist")
    (ok (string= (s+ +u+ "INC") (canvas-url 'inc)))
    (ok (string= (s+ +u+ "INC?n=5&m=2&k=z")
                 (link-url (canvas-link 'inc :n 1 :m 2) :n 5 :k "z")))))

(deftest canvas-url-needs-resolver
  (let ((*canvas-resolver* nil))
    (ok (signals (canvas-url 'inc) 'canvas-no-resolver))
    (ok (signals (link-url (canvas-link 'inc)) 'canvas-no-resolver))
    (ok (typep (make-condition 'canvas-no-resolver) 'canvas-error))))

(deftest path-resolver-urls
  (let ((*canvas-resolver* (make-path-resolver :prefix "/canvas/s1/")))
    (ok (string= (format nil "/canvas/s1/CANVAS-PROTOCOL%2FTESTS::PEOPLE?q=a%20b%26c%2F%C3%A9&page=2")
                 (canvas-url 'people :q (format nil "a b&c/~C" (code-char 233)) :page 2)))
    (ok (string= "/canvas/s1/CANVAS-PROTOCOL%2FTESTS::weird%20name" (canvas-url '|weird name|)))
    (ok (string= "/canvas/s1/CANVAS-PROTOCOL%2FTESTS::INC?x=-._~%3D%3F%23"
                 (canvas-url 'inc :x "-._~=?#"))))
  (let ((*canvas-resolver* (make-path-resolver)))
    (ok (string= "/canvas/CANVAS-PROTOCOL%2FTESTS::INC" (canvas-url 'inc)))
    (ok (string= "/canvas-assets/htmx.min.js" (resolve-asset-url *canvas-resolver* "htmx.min.js"))))
  (ok (string= "/s/canvas.css" (resolve-asset-url (make-path-resolver :asset-prefix "/s/") "canvas.css")))
  (ok (string= "/p/" (path-resolver-prefix (make-path-resolver :prefix "/p/")))))

(deftest freeze-resolver-gives-nil
  (let ((r (make-freeze-resolver)))
    (ok (null (resolve-canvas-url r 'inc '(("n" . "1")))))
    (ok (null (resolve-asset-url r "htmx.min.js")))
    (let ((*canvas-resolver* r))
      (ok (null (canvas-url 'inc :n 1))))))

;;; hx helpers

(deftest hx-get-plist
  (with-fake ()
    (ok (equal (list :hx-get (s+ +u+ "INC?n=1") :hx-target "#c" :hx-swap "outerHTML" :hx-trigger "click")
               (hx-get (canvas-link 'inc :n 1) :target "#c" :swap :outer-html :trigger "click")))
    (ok (equal (list :hx-post (s+ +u+ "SAVE")) (hx-post (canvas-link 'save))))
    (ok (equal '(:hx-get "/x") (hx-get "/x")))))

(deftest hx-key-order
  (with-fake ()
    (ok (equal (list :hx-get (s+ +u+ "INC") :hx-target "#t" :hx-swap "innerHTML" :hx-trigger "load"
                     :hx-select "#s" :hx-include "#f" :hx-vals "{\"a\":\"1\"}" :hx-push-url "true"
                     :hx-confirm "Sure?" :hx-indicator "#i")
               (hx-get (canvas-link 'inc) :indicator "#i" :confirm "Sure?" :push-url t :vals '(:a "1")
                                          :include "#f" :select "#s" :trigger "load" :swap :inner-html
                                          :target "#t")))
    (ok (equal (list :hx-post (s+ +u+ "SAVE") :hx-push-url "/next")
               (hx-post (canvas-link 'save) :push-url "/next")))))

(defparameter +vals-json+
  (format nil "{\"a\":\"1\",\"b\":\"x\\\"y\\\\z<\",\"n\":\"2\",\"nl\":\"a\\nb\"}")
  "json-protocol's (default backend) bytes for the normalized :vals below.")

(deftest hx-vals-json
  ;; The expectation is json-protocol's own output: no private encoder.
  (ok (string= +vals-json+
               (stack-json:encode (list (cons "a" "1") (cons "b" "x\"y\\z<") (cons "n" "2")
                                        (cons "nl" (format nil "a~%b"))))))
  (ok (string= "{\"c\":\"\\u0001\"}" (stack-json:encode (list (cons "c" (string (code-char 1)))))))
  (with-fake ()
    (ok (string= +vals-json+
                 (getf (hx-post (canvas-link 'save)
                                :vals (list :a "1" :b "x\"y\\z<" :n 2 :nl (format nil "a~%b")))
                       :hx-vals)))
    (ok (string= "{\"c\":\"\\u0001\"}"
                 (getf (hx-post (canvas-link 'save) :vals (list :c (string (code-char 1)))) :hx-vals)))
    (ok (equal (list :hx-post (s+ +u+ "SAVE")) (hx-post (canvas-link 'save) :vals '(:a nil))))
    (ok (contains "hx-vals=\"{&quot;b&quot;:&quot;&lt;&quot;}\""
                  (to-html (el :button (hx-post (canvas-link 'save) :vals '(:b "<")) "Go"))))))

(deftest hx-swap-values
  (loop for (kw . str) in '((:inner-html . "innerHTML") (:outer-html . "outerHTML")
                            (:before-begin . "beforebegin") (:after-begin . "afterbegin")
                            (:before-end . "beforeend") (:after-end . "afterend")
                            (:delete . "delete") (:none . "none"))
        do (ok (string= str (hx-swap-value kw)) str))
  (ok (equal +htmx-swap-styles+ (remove-duplicates +htmx-swap-styles+ :key #'car)))
  (ok (string= "outerHTML transition:true" (hx-swap-value "outerHTML transition:true")))
  (ok (signals (hx-swap-value "sideways") 'canvas-error))
  (ok (signals (hx-swap-value :sideways) 'canvas-error))
  (ok (signals (with-fake () (hx-get (canvas-link 'inc) :swap :sideways)) 'canvas-error)))

(deftest hx-refusals
  (with-fake ()
    (ok (signals (hx-get (canvas-link 'inc) :trigger "click[ctrlKey]") 'canvas-csp-error))
    (ok (signals (hx-get (canvas-link 'inc) :trigger (format nil "click~%x")) 'canvas-error))
    (ok (signals (hx-get "javascript:alert(1)") 'canvas-csp-error))
    (ok (signals (hx-get " JAVASCRIPT:alert(1)") 'canvas-csp-error))
    (ok (signals (hx-post (canvas-link 'save) :vals '(:a 1.5)) 'canvas-error))))

(deftest hx-nil-link-is-inert
  (let ((*canvas-resolver* (make-freeze-resolver)))
    (ok (null (hx-get (canvas-link 'inc) :target "#x")))
    (ok (null (hx-post (canvas-link 'save) :vals '(:a "1"))))
    (ok (string= "<button>Go</button>" (to-html (el :button (hx-get (canvas-link 'inc)) "Go"))))))

;;; Response headers (data only)

(deftest response-headers-order
  (ok (equal '(("HX-Redirect" . "/x") ("HX-Reswap" . "innerHTML") ("HX-Retarget" . "#errors")
               ("HX-Trigger" . "saved"))
             (htmx-response-headers :trigger "saved" :retarget "#errors" :reswap :inner-html
                                    :redirect "/x")))
  (ok (equal '(("HX-Location" . "/l") ("HX-Push-Url" . "/p") ("HX-Refresh" . "true")
               ("HX-Replace-Url" . "/r") ("HX-Reselect" . "#s") ("HX-Trigger-After-Settle" . "a")
               ("HX-Trigger-After-Swap" . "b"))
             (htmx-response-headers :trigger-after-swap "b" :trigger-after-settle "a" :reselect "#s"
                                    :replace-url "/r" :refresh t :push-url "/p" :location "/l")))
  (ok (null (htmx-response-headers))))

(deftest response-headers-triggers
  (ok (equal '(("HX-Trigger" . "a, b")) (htmx-response-headers :trigger '("a" "b"))))
  ;; Trigger detail JSON is json-protocol's output for the alist.
  (ok (string= "{\"saved\":\"row \\\"3\\\"\",\"done\":\"\"}"
               (stack-json:encode '(("saved" . "row \"3\"") ("done" . "")))))
  (ok (equal '(("HX-Trigger" . "{\"saved\":\"row \\\"3\\\"\",\"done\":\"\"}"))
             (htmx-response-headers :trigger '(:saved "row \"3\"" :done ""))))
  (ok (equal '(("HX-Reswap" . "outerHTML scroll:top"))
             (htmx-response-headers :reswap "outerHTML scroll:top"))))

(deftest response-headers-refusals
  (ok (signals (htmx-response-headers :redirect (format nil "/x~C~CSet-Cookie: a=b" #\Return #\Newline))
               'canvas-error))
  (ok (signals (htmx-response-headers :retarget (format nil "#a~%b")) 'canvas-error))
  (ok (signals (htmx-response-headers :trigger "a b") 'canvas-error))
  (ok (signals (htmx-response-headers :trigger '("ok" "bad;name")) 'canvas-error))
  (ok (signals (htmx-response-headers :reswap :sideways) 'canvas-error)))
