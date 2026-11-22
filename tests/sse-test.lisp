(in-package #:canvas-protocol/tests)

(deftest sse-attribute-helpers
  (with-fake ()
    (ok (equal (list :hx-ext "sse" :sse-connect (s+ +u+ "TICKS")) (sse-connect (canvas-link 'ticks))))
    (ok (equal (list :hx-ext "sse" :sse-connect (s+ +u+ "TICKS?room=a"))
               (sse-connect (canvas-link 'ticks :room "a")))))
  (ok (equal '(:sse-swap "tick") (sse-swap "tick")))
  (ok (equal '(:sse-swap "tick" :hx-target "#log" :hx-swap "beforeend")
             (sse-swap "tick" :swap :before-end :target "#log")))
  (ok (equal '(:sse-close "done") (sse-close "done")))
  (let ((*canvas-resolver* (make-freeze-resolver)))
    (ok (null (sse-connect (canvas-link 'ticks))))))

(deftest sse-event-names
  (dolist (good '("tick" "a.b" "ns:ev" "x_1-2"))
    (ok (equal (list :sse-swap good) (sse-swap good)) good))
  (dolist (bad (list "" "a b" (format nil "x~%y") (format nil "x~Cy" #\Return) "a,b"
                     (string (code-char 233))))
    (ok (signals (sse-swap bad) 'canvas-error) (format nil "~S" bad))
    (ok (signals (sse-close bad) 'canvas-error))
    (ok (signals (fragment-event bad "x") 'canvas-error))))

(deftest fragment-event-block
  (let ((e (with-fake () (fragment-event "tick" (el :p nil "<1>") :id "7"))))
    (ok (stack-sse:sse-event-p e))
    (ok (string= "tick" (stack-sse:sse-event-type e)))
    (ok (string= "7" (stack-sse:sse-event-id e)))
    (ok (string= "<p>&lt;1&gt;</p>" (stack-sse:sse-event-data e)))
    (ok (string= (format nil "id: 7~%event: tick~%data: <p>&lt;1&gt;</p>~%~%")
                 (stack-sse:encode-sse-event e))))
  (let ((e (with-fake () (fragment-event "log" (code-block (format nil "a~%b"))))))
    (ok (null (stack-sse:sse-event-id e)))
    (ok (string= (format nil "event: log~%data: <pre><code>a~%data: b</code></pre>~%~%")
                 (stack-sse:encode-sse-event e))))
  (ok (signals (fragment-event "tick" "x" :id (format nil "1~%event: evil")) 'canvas-error)))
