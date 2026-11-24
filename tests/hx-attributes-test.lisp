(in-package #:canvas-protocol/tests)

;;; HX2 (0.2.0): hx-trigger modifiers, key bindings without eval, hx-sync,
;;; hx-swap-oob, hx-vals through RENDER. Rules: the headers of src/htmx.lisp
;;; and src/page.lisp, and assets/canvas-keys.js.

(defun editor (&key key) key)
(defun search-box (&key q) q)

(defun key-canvas ()
  "An editor panel bound to keys: queued, so keystrokes reach the server in order."
  (el :section (list* :id "score"
                      (hx-post (canvas-link 'editor) :target "#score" :swap :outer-html
                                                     :sync :queue-all
                                                     :keys '("ArrowUp" "Ctrl+[")))
      "bars"))

;;; hx-trigger

(deftest trigger-modifiers-pass-through
  ;; Already in 0.1: a trigger string is kept as given, modifiers included.
  (with-fake ()
    (dolist (trigger '("input changed delay:300ms" "keydown from:body" "load, every 5s"
                       "click once consume" "revealed throttle:1s" "canvas:key queue:all"))
      (let ((plist (hx-get (canvas-link 'search-box) :trigger trigger)))
        (ok (equal (list :hx-get (s+ +u+ "SEARCH-BOX") :hx-trigger trigger) plist) trigger)
        (ok (null (csp-violations (el :input plist))) trigger)))))

(deftest trigger-filters-stay-refused
  ;; htmx 2.0.11 with allowEval false treats a [filter] as always true.
  (with-fake ()
    (let ((c (handler-case (hx-get (canvas-link 'editor)
                                   :trigger "keydown[key=='ArrowUp'] from:body")
               (canvas-csp-error (c) c))))
      (ok (typep c 'canvas-csp-error))
      (ok (equal '(:eval-trigger) (mapcar (lambda (v) (getf v :kind)) (csp-error-violations c))))))
  ;; A filter in a raw tree (hand-built, or from another library) is reported.
  (ok (equal (list (list :kind :eval-trigger :element "div" :attribute "hx-trigger")
                   (list :kind :eval-trigger :element "p" :attribute "data-hx-trigger"))
             (csp-violations (list (raw "div" '(("hx-trigger" . "keydown[key=='ArrowUp'] from:body")))
                                   (raw "p" '(("data-hx-trigger" . "keyup[ctrlKey]")))))))
  (ok (signals (with-fake () (page (raw "div" '(("hx-trigger" . "keydown[key=='Escape']")))))
               'canvas-csp-error)
      "PAGE refuses it"))

;;; hx-sync

(deftest hx-sync-values
  (loop for (kw . str) in '((:drop . "this:drop") (:abort . "this:abort")
                            (:replace . "this:replace") (:queue . "this:queue")
                            (:queue-first . "this:queue first") (:queue-last . "this:queue last")
                            (:queue-all . "this:queue all"))
        do (ok (string= str (hx-sync-value kw)) str))
  (ok (equal '(:drop :abort :replace :queue :queue-first :queue-last :queue-all)
             (mapcar #'car +htmx-sync-strategies+)))
  (dolist (s '("this" "closest form" "#editor:abort" "closest form:queue last"
               " this : queue all " "find .panel:replace"))
    (ok (string= s (hx-sync-value s)) s)))

(deftest hx-sync-refusals
  (dolist (bad (list :sideways "" "  " ":drop" " :abort" "this:" "this:sideways"
                     "this:queue:all" "this:queue everything" "this:queueall"
                     (format nil "this~%:drop") (format nil "this:drop~C" #\Return)
                     42 nil '(:queue-all)))
    (ok (signals (hx-sync-value bad) 'canvas-error) (format nil "~S" bad))))

(deftest hx-sync-in-plists
  (with-fake ()
    (ok (equal (list :hx-post (s+ +u+ "EDITOR") :hx-target "#score" :hx-indicator "#busy"
                     :hx-sync "this:queue all")
               (hx-post (canvas-link 'editor) :sync :queue-all :indicator "#busy" :target "#score")))
    (ok (equal (list :hx-get (s+ +u+ "SEARCH-BOX") :hx-trigger "input changed delay:300ms"
                     :hx-sync "closest form:abort")
               (hx-get (canvas-link 'search-box) :trigger "input changed delay:300ms"
                                                 :sync "closest form:abort")))
    (ok (signals (hx-get (canvas-link 'editor) :sync :sideways) 'canvas-error))
    (ok (string= (s+ "<form hx-sync=\"this:replace\"><label><input type=\"text\" name=\"q\"></label></form>")
                 (to-html (form (text-input "q") :attrs (list :hx-sync (hx-sync-value :replace)))))))
  (let ((*canvas-resolver* (make-freeze-resolver)))
    (ok (null (hx-post (canvas-link 'editor) :sync :queue-all)) "NIL link: no attributes")
    (ok (signals (hx-post (canvas-link 'editor) :sync "a:b:c") 'canvas-error)
        "options are validated before the NIL link")))

;;; hx-swap-oob

(deftest hx-swap-oob-values
  (ok (equal '(:hx-swap-oob "true") (hx-swap-oob)))
  (ok (equal '(:hx-swap-oob "innerHTML") (hx-swap-oob :swap :inner-html)))
  (ok (equal '(:hx-swap-oob "beforeend") (hx-swap-oob :swap "beforeend")))
  (ok (equal '(:hx-swap-oob "beforeend:#log") (hx-swap-oob :swap :before-end :target "#log")))
  (ok (equal '(:hx-swap-oob "outerHTML:#status") (hx-swap-oob :target "#status")))
  (ok (equal '(:hx-swap-oob "delete:#toast") (hx-swap-oob :swap :delete :target "#toast")))
  (ok (equal '(:hx-swap-oob "innerHTML:.panel > p") (hx-swap-oob :swap "innerHTML" :target ".panel > p"))))

(deftest hx-swap-oob-refusals
  (dolist (args (list '(:swap :sideways) '(:swap "sideways") '(:swap "innerHTML show:top")
                      '(:swap "outerHTML:#x") '(:swap "innerHTML ") '(:swap 3) '(:target "")
                      '(:target 7) (list :target (format nil "#a~%b"))
                      (list :swap :inner-html :target (format nil "#a~Cb" #\Return))))
    (ok (signals (apply #'hx-swap-oob args) 'canvas-error) (format nil "~S" args))))

(deftest oob-response-updates-several-panels
  ;; One response: the main target's new content plus two out-of-band panels.
  (let ((response (list (el :div '(:id "score") "bar 3")
                        (card "2 of 8 selected" :id "status" :attrs (hx-swap-oob))
                        (el :ul (list* :id "log" (hx-swap-oob :swap :before-end))
                            (el :li nil "moved <up>")))))
    (ok (string= (s+ "<div id=\"score\">bar 3</div>"
                     "<article id=\"status\" hx-swap-oob=\"true\">2 of 8 selected</article>"
                     "<ul id=\"log\" hx-swap-oob=\"beforeend\"><li>moved &lt;up&gt;</li></ul>")
                 (html response)))
    (ok (null (csp-violations response)))))

;;; Key chords

(defparameter +chord-rows+
  '(("ArrowUp" . "ArrowUp") ("arrowup" . "ArrowUp") ("ARROWDOWN" . "ArrowDown")
    ("z" . "z") ("Z" . "z") ("ctrl+z" . "Ctrl+z") ("Control+Z" . "Ctrl+z")
    ("shift+ctrl+z" . "Ctrl+Shift+z") ("Meta+Alt+Shift+Ctrl+k" . "Ctrl+Alt+Shift+Meta+k")
    ("Shift+Space" . "Shift+Space") ("space" . "Space") ("Plus" . "Plus")
    ("ctrl+plus" . "Ctrl+Plus") ("?" . "?") ("Ctrl+/" . "Ctrl+/") ("alt+1" . "Alt+1")
    ("Shift+Tab" . "Shift+Tab") ("f5" . "F5") ("Shift+F12" . "Shift+F12")
    ("Delete" . "Delete") ("escape" . "Escape") ("ENTER" . "Enter") ("backspace" . "Backspace")
    ("pageup" . "PageUp") ("Shift+ArrowUp" . "Shift+ArrowUp") ("~" . "~")
    ("Ctrl+[" . "Ctrl+[") ("Alt+]" . "Alt+]") ("META+ArrowLeft" . "Meta+ArrowLeft")
    ("Shift+a" . "Shift+a") ("Shift+A" . "Shift+a") ("0" . "0") ("#" . "#"))
  "KEY-CHORD input -> canonical chord, as canvas-keys.js reports the key press.")

(deftest key-chord-canonical
  (loop for (in . out) in +chord-rows+
        do (ok (equal out (key-chord in)) (format nil "~S -> ~S" in out)))
  (loop for (nil . out) in +chord-rows+
        do (ok (equal out (key-chord out)) (format nil "~S is canonical" out))))

(deftest key-chord-refusals
  (dolist (bad (list "" "+" "Ctrl+" "Ctrl++" "+z" "Shift+?" "Shift+/" "Shift+1" "Shift+Plus"
                     "Ctrl+Ctrl+z" "control+ctrl+z" "Shift+shift+a" "Hyper+z" "Ctrl+Alt"
                     "ArrowSideways" "Esc" "ab" "F13" " " "Ctrl+ " (string (code-char 233))
                     (format nil "a~%") 'z 42 nil))
    (ok (signals (key-chord bad) 'canvas-error) (format nil "~S" bad)))
  (ok (equal +canvas-key-event+ "canvas:key"))
  (ok (subsetp '("ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight" "Space" "Plus" "Enter" "Escape")
               +canvas-key-names+ :test #'string=)))

;;; Key bindings on hx-get / hx-post

(deftest hx-keys-plist
  (with-fake ()
    (ok (equal (list :hx-post (s+ +u+ "EDITOR") :hx-target "#score" :hx-trigger "canvas:key"
                     :hx-sync "this:queue all" :data-canvas-keys "ArrowUp ArrowDown Ctrl+z")
               (hx-post (canvas-link 'editor) :target "#score" :sync :queue-all
                                              :keys '("ArrowUp" "arrowdown" "ctrl+Z" "ArrowUp"))))
    (ok (equal (list :hx-get (s+ +u+ "EDITOR") :hx-trigger "canvas:key" :data-canvas-keys "Escape")
               (hx-get (canvas-link 'editor) :keys "escape")))
    (ok (equal (list :hx-post (s+ +u+ "EDITOR") :hx-trigger "canvas:key throttle:50ms"
                     :data-canvas-keys "ArrowLeft")
               (hx-post (canvas-link 'editor) :keys '("ArrowLeft") :trigger "canvas:key throttle:50ms")))
    (ok (equal (list :hx-post (s+ +u+ "EDITOR") :hx-trigger "click, canvas:key"
                     :data-canvas-keys "Enter")
               (hx-post (canvas-link 'editor) :keys "Enter" :trigger "click, canvas:key")))
    (ok (equal (list :hx-post (s+ +u+ "EDITOR")) (hx-post (canvas-link 'editor) :keys nil))
        "no keys, no key attributes")
    (dolist (trigger '("click" "keydown from:body" "canvas:keyup" "load, keydown"))
      (ok (signals (hx-post (canvas-link 'editor) :keys "ArrowUp" :trigger trigger) 'canvas-error)
          trigger))
    (ok (signals (hx-post (canvas-link 'editor) :keys '("ArrowUp" "Shift+?")) 'canvas-error))
    (ok (signals (hx-post (canvas-link 'editor) :keys 42) 'canvas-error)))
  (let ((*canvas-resolver* (make-freeze-resolver)))
    (ok (null (hx-post (canvas-link 'editor) :keys "ArrowUp")) "NIL link: no attributes")))

(deftest keys-render-csp-clean
  (let ((tree (with-fake () (key-canvas))))
    (ok (string= (s+ "<section id=\"score\" hx-post=\"" +u+ "EDITOR\" hx-target=\"#score\""
                     " hx-swap=\"outerHTML\" hx-trigger=\"canvas:key\" hx-sync=\"this:queue all\""
                     " data-canvas-keys=\"ArrowUp Ctrl+[\">bars</section>")
                 (to-html tree)))
    (ok (null (csp-violations tree)) "a [ in data-canvas-keys is not a trigger filter")
    ;; A frozen file loads no script: the bindings go with the hx attributes.
    (ok (string= "<section id=\"score\">bars</section>" (to-html (neutralize tree))))))

(deftest page-loads-keys-script
  (ok (string= (s+ +head-start+ +htmx-config-meta+
                   "<title>canvas</title><link rel=\"stylesheet\" href=\"/a/canvas.css\">"
                   "<script src=\"/a/htmx.min.js\"></script><script src=\"/a/htmx-ext-sse.min.js\"></script>"
                   "<script src=\"/a/canvas-keys.js\"></script>"
                   "</head><body><p>hi</p></body></html>")
               (with-fake () (to-html (page (el :p nil "hi") :keys t)))))
  (ok (signals (with-fake () (page "x" :keys t :htmx nil)) 'canvas-error))
  (ok (signals (with-fake () (page "x" :keys :yes)) 'canvas-error))
  (let ((doc (with-fake () (page (key-canvas) :keys t))))
    (ok (null (csp-violations doc)))
    (ok (= 3 (count "script" (element-names doc) :test #'string=))))
  (ok (not (contains "canvas-keys" (with-fake () (to-html (page "x"))))) "KEYS NIL: the 0.1 page"))

(deftest keys-script-asset
  ;; Given with the contract: the script evaluates nothing and writes no HTML.
  (let ((js (map 'string #'code-char (asset-bytes "canvas-keys.js"))))
    (ok (every (lambda (c) (< (char-code c) 128)) js) "ASCII")
    (ok (contains "data-canvas-keys" js))
    (ok (contains "\"canvas:key\"" js))
    (ok (contains "htmx:configRequest" js))
    (ok (contains "parameters[\"key\"]" js))
    (dolist (forbidden '("eval(" "Function(" "innerHTML" "outerHTML" "insertAdjacentHTML"
                         "document.write" "setTimeout(" "setInterval(" "import(" "fetch("
                         "XMLHttpRequest" "javascript:"))
      (ok (not (contains forbidden js)) forbidden))
    (ok (string= (sha256-hex (asset-bytes "canvas-keys.js")) (asset-sha256 "canvas-keys.js")))))

;;; hx-vals and hx-include / hx-indicator through RENDER

(deftest hx-vals-through-render
  ;; json-protocol encodes, html-protocol escapes: no private encoder.
  (with-fake ()
    (let* ((note (format nil "C#4 \"<tied>\" & more"))
           (plist (hx-post (canvas-link 'editor) :vals (list :op "move" :note note)
                                                 :include "#selection" :indicator "#busy"))
           (button (el :button plist "Go")))
      (ok (string= (stack-json:encode (list (cons "op" "move") (cons "note" note)))
                   (getf plist :hx-vals)))
      (ok (string= (s+ "<button hx-post=\"" +u+ "EDITOR\" hx-include=\"#selection\" hx-vals=\""
                       "{&quot;op&quot;:&quot;move&quot;,&quot;note&quot;:&quot;C#4 \\&quot;&lt;tied>\\&quot; &amp; more&quot;}"
                       "\" hx-indicator=\"#busy\">Go</button>")
                   (to-html button)))
      (ok (null (csp-violations button))))))
