(in-package #:canvas-protocol/tests)

(defun two-ps () (list (el :p nil "a") (el :p nil "b")))

(deftest stack-layout-html
  (ok (string= "<div data-layout=\"stack\"><p>a</p><p>b</p></div>" (html (stack (two-ps)))))
  (ok (string= "<div id=\"s1\" data-layout=\"row\" data-gap=\"s\"><p>a</p><p>b</p></div>"
               (html (stack (two-ps) :gap :s :direction :horizontal :id "s1"))))
  (ok (string= "<div data-layout=\"stack\" data-gap=\"none\" aria-label=\"x\">a</div>"
               (html (stack "a" :gap :none :attrs '(:aria-label "x")))))
  (ok (string= "<div data-layout=\"stack\" data-gap=\"l\"></div>" (html (stack nil :gap :l))))
  (ok (signals (stack nil :gap :huge) 'canvas-error))
  (ok (signals (stack nil :direction :diagonal) 'canvas-error))
  (ok (signals (stack nil :attrs '(:onclick "x")) 'canvas-csp-error)))

(deftest grid-html
  (ok (string= "<div data-layout=\"grid\" data-columns=\"3\"><p>1</p></div>"
               (html (grid (list (el :p nil "1")) :columns 3))))
  (ok (string= "<div data-layout=\"grid\" data-columns=\"2\" data-gap=\"s\">x</div>"
               (html (grid "x" :gap :s))))
  (ok (signals (grid nil :columns 0) 'canvas-error))
  (ok (signals (grid nil :columns 7) 'canvas-error))
  (ok (signals (grid nil :columns "2") 'canvas-error)))

(deftest card-html
  (ok (string= "<article><header><h3>T&lt;</h3></header><p>body</p><footer>f</footer></article>"
               (html (card (el :p nil "body") :title "T<" :footer "f"))))
  (ok (string= "<article id=\"c\"><p>a</p><p>b</p></article>" (html (card (two-ps) :id "c")))))

(deftest details-and-fieldset-html
  (ok (string= "<details open=\"\"><summary>More</summary><p>x</p></details>"
               (html (details "More" (el :p nil "x") :open t))))
  (ok (string= "<details><summary>&lt;s&gt;</summary></details>" (html (details "<s>" nil))))
  (ok (string= "<fieldset><legend>Who</legend><p>a</p><p>b</p></fieldset>"
               (html (fieldset (two-ps) :legend "Who"))))
  (ok (string= "<fieldset>x</fieldset>" (html (fieldset "x")))))

(deftest tabs-html
  (ok (string= (s+ "<section id=\"t\"><nav role=\"tablist\">"
                   "<button type=\"button\" role=\"tab\" aria-selected=\"false\" hx-get=\""
                   +u+ "TAB-DEMO?tab=a\" hx-target=\"#t\" hx-swap=\"outerHTML\">Alpha</button>"
                   "<button type=\"button\" role=\"tab\" aria-selected=\"true\" hx-get=\""
                   +u+ "TAB-DEMO?tab=b\" hx-target=\"#t\" hx-swap=\"outerHTML\">Beta</button>"
                   "</nav><div role=\"tabpanel\"><p>beta</p></div></section>")
               (html (tabs '(("a" . "Alpha") ("b" . "Beta")) :id "t" :active "b"
                           :link (canvas-link 'tab-demo)
                           :content (el :p nil "beta"))))))

(deftest tabs-without-link-and-defaults
  (ok (string= (s+ "<section id=\"t\"><nav role=\"tablist\">"
                   "<button type=\"button\" role=\"tab\" aria-selected=\"true\">One</button>"
                   "<button type=\"button\" role=\"tab\" aria-selected=\"false\">Two</button>"
                   "</nav><div role=\"tabpanel\">c</div></section>")
               (html (tabs '("One" "Two") :id "t" :content "c"))))
  (ok (signals (tabs '("One") :active "One") 'canvas-error) "id required")
  (ok (signals (tabs '("One") :id "t" :active "Nope") 'canvas-error))
  (ok (signals (tabs '() :id "t") 'canvas-error)))

(deftest tabs-link-keeps-params
  (ok (contains (s+ "hx-get=\"" +u+ "TAB-DEMO?x=1&amp;tab=a\"")
                (html (tabs '("a") :id "t" :link (canvas-link 'tab-demo :x 1))))))

(deftest code-block-html
  (ok (string= "<pre><code data-language=\"lisp\">(defun f () \"&lt;x&gt;\")</code></pre>"
               (html (code-block "(defun f () \"<x>\")" :language "lisp"))))
  (ok (string= "<pre><code>a&amp;b</code></pre>" (html (code-block "a&b"))))
  (ok (signals (code-block "x" :language "lisp\" onclick=\"x") 'canvas-error)))

(deftest kv-list-html
  (ok (string= "<dl><dt>Name</dt><dd>Ann</dd><dt>Age</dt><dd>3</dd></dl>"
               (html (kv-list '(("Name" . "Ann") ("Age" . 3))))))
  (ok (string= "<dl><dt>&lt;k&gt;</dt><dd><b>v</b></dd></dl>"
               (html (kv-list (list (cons "<k>" (el :b nil "v")))))))
  (ok (string= "<dl></dl>" (html (kv-list nil)))))

(deftest badge-html
  (ok (string= "<span data-badge=\"ok\">ok</span>" (html (badge "ok" :status :ok))))
  (ok (string= "<span data-badge=\"neutral\">&lt;n&gt;</span>" (html (badge "<n>"))))
  (dolist (s '(:info :warn :error))
    (ok (contains (format nil "data-badge=\"~(~A~)\"" s) (html (badge "x" :status s)))))
  (ok (signals (badge "x" :status :purple) 'canvas-error)))

(deftest live-region-html
  (ok (string= (s+ "<div id=\"clock\" hx-ext=\"sse\" sse-connect=\"" +u+ "TICKS\" sse-swap=\"tick\""
                   " aria-live=\"polite\">waiting</div>")
               (html (live-region (canvas-link 'ticks) "tick" :id "clock" :content "waiting"))))
  (ok (string= (s+ "<div hx-ext=\"sse\" sse-connect=\"" +u+ "TICKS\" sse-swap=\"log\""
                   " hx-swap=\"beforeend\" aria-live=\"polite\"></div>")
               (html (live-region (canvas-link 'ticks) "log" :swap :before-end))))
  (ok (string= "<div aria-live=\"polite\">x</div>"
               (let ((*canvas-resolver* (make-freeze-resolver)))
                 (to-html (live-region (canvas-link 'ticks) "tick" :content "x")))))
  (ok (signals (live-region (canvas-link 'ticks) "bad name") 'canvas-error)))
