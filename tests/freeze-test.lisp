(in-package #:canvas-protocol/tests)

(defun frozen-demo (&key (n "2"))
  (stack (list (el :p nil (format nil "n=~A" n))
               (el :button (hx-get (canvas-link 'frozen-demo :n "3") :target "#x") "More"))))

(defvar *counter* 0)
(defun stateful-canvas () (el :p nil (princ-to-string *counter*)))

(defun raises-canvas () (error "boom"))

(defclass test-chart-backend (chart-backend)
  ((seen :initform nil :accessor backend-seen)))

(defmethod render-chart ((backend test-chart-backend) chart &key)
  (push chart (backend-seen backend))
  (el :svg '(:role "img" :viewbox "0 0 10 10") (el :title nil (or (chart-title chart) "chart"))))

(defun chart-canvas ()
  (card (make-chart :line :title "Speed" :series (list (make-series "a" '((0 . 1) (1 . 2)))))))

(deftest freeze-golden
  (ok (string= (s+ +head-start+ "<title>Demo</title><style>" (canvas-stylesheet) "</style></head>"
                   "<body data-canvas-frozen=\"\"><div data-layout=\"stack\"><p>n=5</p><button>More</button></div>"
                   "</body></html>")
               (freeze 'frozen-demo :args '(:n "5") :title "Demo"))))

(deftest freeze-defaults
  (let ((out (freeze 'frozen-demo)))
    (ok (contains "<title>frozen-demo</title>" out))
    (ok (contains "<p>n=2</p>" out)))
  (ok (contains "<title>canvas</title>" (freeze #'frozen-demo)))
  (ok (contains "<p>n=4</p>" (freeze #'frozen-demo :args '(:n "4")))))

(deftest freeze-ignores-outer-resolver
  (with-fake (r)
    (let ((out (freeze 'frozen-demo)))
      (ok (null (fake-calls r)))
      (ok (not (contains "/t/" out)))
      (ok (not (contains "/a/" out))))))

(deftest freeze-deterministic
  (ok (string= (freeze 'gallery :args '(:sort "name")) (freeze 'gallery :args '(:sort "name"))))
  (let ((*counter* 1))
    (let ((a (freeze 'stateful-canvas)))
      (ok (string= a (freeze 'stateful-canvas)))
      (setf *counter* 2)
      (ok (contains "<p>2</p>" (freeze 'stateful-canvas)) "live image state"))))

(deftest freeze-self-contained
  (let ((out (freeze 'gallery :args '(:sort "-age" :page "2"))))
    (dolist (bad '("http:" "https:" "\"//" "<script" "<link" " hx-" " sse-" "src=" "href=" "url(" "@import"))
      (ok (not (contains bad out)) (format nil "no ~A" bad)))
    (ok (contains "<body data-canvas-frozen=\"\">" out))
    (ok (contains "<table><caption>People</caption>" out))
    (ok (contains "aria-sort=\"descending\"" out))))

(deftest freeze-keeps-chart-svg
  (let ((*chart-backend* (make-instance 'test-chart-backend)))
    (let ((out (freeze 'chart-canvas)))
      (ok (contains "<svg role=\"img\" viewbox=\"0 0 10 10\"><title>Speed</title></svg>" out)))))

(deftest freeze-refuses
  (ok (signals (freeze 'no-such-canvas-function-xyz) 'canvas-error))
  (ok (signals (freeze 42) 'canvas-error))
  (ok (signals (freeze 'frozen-demo :args '(:n)) 'canvas-error))
  (ok (signals (freeze 'raises-canvas) 'simple-error) "canvas errors propagate"))

(defun hostile-tree ()
  (raw "div" nil
       (raw "script" nil "alert(1)")
       (raw "a" '(("href" . "https://evil.example/x")) "ext")
       (raw "a" '(("href" . "#top")) "top")
       (raw "img" '(("src" . "data:image/png;base64,AAAA")))
       (raw "img" '(("src" . "https://evil.example/p.png") ("alt" . "p")))
       (raw "link" '(("rel" . "stylesheet") ("href" . "https://cdn.example/x.css")))
       (raw "iframe" '(("src" . "https://evil.example/")))
       (raw "button" '(("hx-get" . "/x") ("onclick" . "steal()") ("style" . "color:red")) "b")
       (raw "form" '(("action" . "https://evil.example/") ("hx-post" . "/y"))
            (raw "input" '(("name" . "q"))))
       (raw "div" '(("hx-ext" . "sse") ("sse-connect" . "/s") ("data-hx-get" . "/z")) "live")
       (raw "style" nil "body{background:url(https://evil.example/t.png)}")))

(deftest neutralize-hostile-tree
  (let* ((tree (hostile-tree))
         (clean (neutralize tree)))
    (ok (string= (s+ "<div><a>ext</a><a href=\"#top\">top</a><img src=\"data:image/png;base64,AAAA\">"
                     "<img alt=\"p\"><button>b</button><form><input name=\"q\"></form><div>live</div></div>")
                 (to-html clean)))
    (ok (contains "alert(1)" (to-html tree)) "input not modified")
    (ok (null (csp-violations clean)))))

(deftest freeze-neutralizes-raw-nodes
  (let ((out (freeze (lambda () (hostile-tree)))))
    (ok (not (contains "evil.example" out)))
    (ok (not (contains "alert(1)" out)))
    (ok (contains "<a href=\"#top\">top</a>" out))))
