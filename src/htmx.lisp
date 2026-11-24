(in-package #:canvas-protocol)

;;; htmx attribute helpers, response headers (as data) and SSE helpers.
;;;
;;; HX-GET / HX-POST LINK &key target swap trigger select include vals push-url
;;; confirm indicator -> attribute plist in exactly this key order:
;;;   :hx-get|:hx-post :hx-target :hx-swap :hx-trigger :hx-select :hx-include
;;;   :hx-vals :hx-push-url :hx-confirm :hx-indicator   (NIL keys omitted)
;;; - LINK: a CANVAS-LINK (resolved with LINK-URL) or a URL string (a
;;;   "javascript:" string -> CANVAS-CSP-ERROR). A link resolving to NIL ->
;;;   the helper returns NIL (no attributes at all).
;;; - SWAP: a keyword of +HTMX-SWAP-STYLES+, or a string whose first
;;;   space-separated token is one of their values (modifiers follow);
;;;   else CANVAS-ERROR.
;;; - TRIGGER: a string; "[" (an eval'd filter) -> CANVAS-CSP-ERROR; CR/LF -> CANVAS-ERROR.
;;; - VALS: a param plist, normalized as in resolver.lisp to an alist
;;;   (name . string), then (stack-json:encode alist): json-protocol under the
;;;   current *JSON-BACKEND* (json-backend-jzon, a dependency, is the default).
;;;   No JSON encoder of our own. Determinism comes from json-protocol: an
;;;   alist encodes in alist order (json-protocol 0.2.2 guarantees it), compact.
;;;   An empty normalized alist -> :hx-vals omitted. The string is not
;;;   HTML-safe by itself ("<" stays raw): html-protocol escapes attribute values.
;;; - PUSH-URL: T -> "true", a string as is.
;;;
;;; 0.2.0 (HX2) adds two keys, so the full order is
;;;   :hx-get|:hx-post :hx-target :hx-swap :hx-trigger :hx-select :hx-include
;;;   :hx-vals :hx-push-url :hx-confirm :hx-indicator :hx-sync :data-canvas-keys
;;; - TRIGGER is unchanged: one string, passed as given, so every htmx
;;;   modifier and several comma-separated triggers work ("input changed
;;;   delay:300ms", "keydown from:body", "load, every 5s", "click once
;;;   consume", "canvas:key queue:all"). A "[" is still refused
;;;   (CANVAS-CSP-ERROR, and CSP-VIOLATIONS reports :eval-trigger for one in
;;;   a raw tree): htmx evaluates an event filter such as
;;;   keydown[key=='ArrowUp'] as a JavaScript expression (the Function
;;;   constructor, through maybeEval), which needs 'unsafe-eval' in the CSP.
;;;   The canvas CSP has none, and with allowEval false (+HTMX-CONFIG+)
;;;   htmx 2.0.11's maybeEval hands back its default, a function that always
;;;   answers true, so a filter would silently match every event. Key
;;;   bindings go through KEYS and the same-origin keymap script instead.
;;; - SYNC: (HX-SYNC-VALUE sync) -> :hx-sync.
;;; - KEYS: a key-chord string or a list of them. Each is canonicalized with
;;;   KEY-CHORD; duplicates after that are dropped (first one kept) and the
;;;   rest joined with single spaces -> :data-canvas-keys. KEYS NIL -> no key
;;;   attribute. With KEYS and no TRIGGER, :hx-trigger is +CANVAS-KEY-EVENT+
;;;   ("canvas:key"). With KEYS and a TRIGGER, the trigger must listen for it:
;;;   one of its comma-separated specs, trimmed, has "canvas:key" as its first
;;;   space-separated token ("canvas:key throttle:50ms", "click, canvas:key");
;;;   otherwise CANVAS-ERROR.
;;;   The event comes from the asset canvas-keys.js (see its header: chords,
;;;   form-field focus, the request parameter "key" = the pressed chord), which
;;;   a page loads with (PAGE body :keys t). Without that script no event fires
;;;   and no request is sent.
;;;
;;; HX-SYNC-VALUE sync -> the hx-sync attribute string (also usable in ATTRS
;;; of a form or container, since htmx inherits hx-sync):
;;; - a keyword of +HTMX-SYNC-STRATEGIES+ -> "this:<strategy>" (:queue-all ->
;;;   "this:queue all");
;;; - a string in the one shape htmx 2.0.11 parses: SELECTOR, or SELECTOR ":"
;;;   STRATEGY. htmx splits at ":" and reads only the first two pieces, so at
;;;   most one ":" is allowed. SELECTOR, trimmed, is non-empty ("this",
;;;   "closest form", "#editor"); STRATEGY, trimmed, is one of the
;;;   +HTMX-SYNC-STRATEGIES+ strings (htmx's default for a bare SELECTOR is
;;;   drop). The string is returned as given;
;;; - CR or LF, or anything else -> CANVAS-ERROR.
;;;
;;; HX-SWAP-OOB &key swap target -> (:hx-swap-oob VALUE), the attribute for the
;;; root of a response fragment that htmx swaps out of band, so that one
;;; response updates several panels:
;;; - neither -> "true" (replace the element with the same id);
;;; - SWAP -> its style: a keyword of +HTMX-SWAP-STYLES+ or one of their
;;;   strings. No modifiers: htmx cuts the value at its first ":", so
;;;   "innerHTML show:top" would name the selector "top";
;;; - TARGET (a CSS selector) -> "<style>:<target>", style default "outerHTML";
;;; - an empty or non-string TARGET, a TARGET with CR or LF, a SWAP with a
;;;   space or a ":", an unknown style -> CANVAS-ERROR.
;;;
;;; KEY-CHORD string -> the canonical chord string canvas-keys.js produces:
;;; - "+"-separated tokens: zero or more modifiers, then exactly one key.
;;;   Modifiers, case-insensitive: "ctrl" or "control" -> Ctrl, "alt" -> Alt,
;;;   "shift" -> Shift, "meta" -> Meta; printed in the order Ctrl, Alt,
;;;   Shift, Meta, each followed by "+".
;;; - Key: a name in +CANVAS-KEY-NAMES+, matched case-insensitively and
;;;   printed as listed ("arrowup" -> "ArrowUp", "space" -> "Space"); or one
;;;   ASCII graphic character other than "+" (code 33..126), a letter printed
;;;   in lower case ("Ctrl+Z" -> "Ctrl+z"). The "+" key is "Plus", the space
;;;   bar "Space".
;;; - Shift with a one-character key that is not a letter -> CANVAS-ERROR: the
;;;   browser reports the shifted character itself ("?", not Shift+/), so the
;;;   chord could never match. Shift with "Plus" -> CANVAS-ERROR likewise.
;;; - A repeated modifier, an empty token ("Ctrl++", "", "Ctrl+"), a key that
;;;   is neither, a non-string -> CANVAS-ERROR.
;;;
;;; HTMX-RESPONSE-HEADERS -> alist ("HX-..." . string) sorted by header name
;;; (string<). Keys -> headers: :location HX-Location, :push-url HX-Push-Url,
;;; :redirect HX-Redirect, :refresh HX-Refresh (T -> "true"), :replace-url
;;; HX-Replace-Url, :reselect HX-Reselect, :reswap HX-Reswap (swap rules),
;;; :retarget HX-Retarget, :trigger HX-Trigger, :trigger-after-settle
;;; HX-Trigger-After-Settle, :trigger-after-swap HX-Trigger-After-Swap.
;;; Trigger values: an event name; a list of names -> "a, b"; a plist
;;; (:event "detail" ...) -> JSON object, encoded as VALS (stack-json:encode of
;;; the alist; details must be strings, else CANVAS-ERROR). Event names follow the
;;; SSE rule below. CR or LF in any value -> CANVAS-ERROR. NIL args omitted.
;;;
;;; SSE: event names (and FRAGMENT-EVENT ids) match [A-Za-z0-9_.:-]+, else CANVAS-ERROR.
;;; (sse-connect link) -> (:hx-ext "sse" :sse-connect url), NIL when the URL is NIL.
;;; (sse-swap event &key target swap) -> (:sse-swap event [:hx-target t] [:hx-swap s]).
;;; (sse-close event) -> (:sse-close event).
;;; (fragment-event event tree &key id) -> stack-sse:sse-event, :event EVENT,
;;; :data (TO-HTML TREE), :id ID.

(defparameter +htmx-swap-styles+
  '((:inner-html . "innerHTML") (:outer-html . "outerHTML")
    (:before-begin . "beforebegin") (:after-begin . "afterbegin")
    (:before-end . "beforeend") (:after-end . "afterend")
    (:delete . "delete") (:none . "none"))
  "Keyword -> htmx hx-swap style.")

(defparameter +htmx-sync-strategies+
  '((:drop . "drop") (:abort . "abort") (:replace . "replace") (:queue . "queue")
    (:queue-first . "queue first") (:queue-last . "queue last") (:queue-all . "queue all"))
  "Keyword -> htmx hx-sync strategy (htmx 2.0.11).")

(defparameter +canvas-key-event+ "canvas:key"
  "The event canvas-keys.js dispatches on an element whose data-canvas-keys lists the chord.")

(defparameter +canvas-key-names+
  '("Enter" "Tab" "Escape" "Backspace" "Delete" "Insert" "Home" "End" "PageUp" "PageDown"
    "ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight" "Space" "Plus"
    "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12")
  "Named keys KEY-CHORD accepts, spelled as canvas-keys.js reports them
(KeyboardEvent.key, with \" \" as Space and \"+\" as Plus).")

(defun %event-name-p (string)
  "STRING matches [A-Za-z0-9_.:-]+ (ASCII only)."
  (%token-p string
            (lambda (c) (or (%ascii-alnum-p c) (find c "_.:-")))
            (lambda (c) (or (%ascii-alnum-p c) (find c "_.:-")))))

(defun %event-name (name)
  (unless (%event-name-p name)
    (%canvas-error "invalid event name ~S (must match [A-Za-z0-9_.:-]+)" name))
  name)

(defun %no-crlf (what string)
  (when (or (find #\Return string) (find #\Newline string))
    (%canvas-error "~A must not contain CR or LF: ~S" what string))
  string)

(defun %string-option (key value)
  (unless (stringp value)
    (%canvas-error "~(~A~) must be a string: ~S" key value))
  value)

(defun hx-swap-value (swap)
  "SWAP keyword or string -> the hx-swap attribute string. Invalid -> CANVAS-ERROR."
  (cond ((keywordp swap)
         (or (cdr (assoc swap +htmx-swap-styles+))
             (%canvas-error "unknown hx-swap style ~S" swap)))
        ((stringp swap)
         (let ((style (subseq swap 0 (or (position #\Space swap) (length swap)))))
           (unless (member style +htmx-swap-styles+ :key #'cdr :test #'string=)
             (%canvas-error "unknown hx-swap style ~S" swap))
           swap))
        (t (%canvas-error "hx-swap must be a keyword or string: ~S" swap))))

(defun %hx-url (verb link)
  (cond ((stringp link)
         (when (%javascript-url-p link)
           (%csp-error :javascript-url nil (string-downcase (symbol-name verb))))
         link)
        (t (link-url link))))

(defun %hx-trigger (trigger)
  (%string-option :trigger trigger)
  (when (find #\[ trigger)
    (%csp-error :eval-trigger nil "hx-trigger"))
  (%no-crlf "hx-trigger" trigger))

(defun %hx-vals (vals)
  (let ((alist (%normalize-params vals)))
    (when alist (stack-json:encode alist))))

(defun %hx-push-url (push-url)
  (if (eq push-url t) "true" (%string-option :push-url push-url)))

(defun hx-sync-value (sync)
  "SYNC keyword or string -> the hx-sync attribute string. See the file header."
  (declare (ignorable sync))
  (%hx2-pending 'hx-sync-value))

(defun hx-swap-oob (&key swap target)
  "(:hx-swap-oob VALUE) for an out-of-band response fragment. See the file header."
  (declare (ignorable swap target))
  (%hx2-pending 'hx-swap-oob))

(defun key-chord (string)
  "The canonical key chord for STRING (\"ctrl+Z\" -> \"Ctrl+z\"). See the file header."
  (declare (ignorable string))
  (%hx2-pending 'key-chord))

(defun %hx-keys (keys trigger)
  "KEYS (a chord or a list of chords) and the caller's TRIGGER ->
(values data-canvas-keys-string effective-trigger). See the file header."
  (declare (ignorable keys trigger))
  (%hx2-pending 'hx-keys))

(defun %hx (verb link target swap trigger select include vals push-url confirm indicator
            &optional sync keys)
  "Shared body of HX-GET / HX-POST: options are validated first, then a NIL URL -> NIL."
  (multiple-value-bind (key-list trigger)
      (if keys (%hx-keys keys trigger) (values nil trigger))
    (let* ((url (%hx-url verb link))
           (pairs (list (cons :hx-target (and target (%string-option :target target)))
                        (cons :hx-swap (and swap (hx-swap-value swap)))
                        (cons :hx-trigger (and trigger (%hx-trigger trigger)))
                        (cons :hx-select (and select (%string-option :select select)))
                        (cons :hx-include (and include (%string-option :include include)))
                        (cons :hx-vals (and vals (%hx-vals vals)))
                        (cons :hx-push-url (and push-url (%hx-push-url push-url)))
                        (cons :hx-confirm (and confirm (%string-option :confirm confirm)))
                        (cons :hx-indicator (and indicator (%string-option :indicator indicator)))
                        (cons :hx-sync (and sync (hx-sync-value sync)))
                        (cons :data-canvas-keys key-list))))
      (when url
        (list* verb url
               (loop for (key . value) in pairs
                     when value nconc (list key value)))))))

(defun hx-get (link &key target swap trigger select include vals push-url confirm indicator
                         sync keys)
  "Attribute plist for an hx-get to LINK. See the file header."
  (%hx :hx-get link target swap trigger select include vals push-url confirm indicator
       sync keys))

(defun hx-post (link &key target swap trigger select include vals push-url confirm indicator
                          sync keys)
  "Attribute plist for an hx-post to LINK. See the file header."
  (%hx :hx-post link target swap trigger select include vals push-url confirm indicator
       sync keys))

(defun %header-value (key value)
  (cond ((stringp value) value)
        ((eq value t) "true")
        (t (%canvas-error "~(~A~) must be a string or T: ~S" key value))))

(defun %trigger-header (value)
  (cond ((stringp value) (%event-name value))
        ((and (consp value) (symbolp (first value)))
         ;; A plist (:event "detail" ...) -> JSON object, encoded by json-protocol.
         (unless (%plist-p value)
           (%canvas-error "trigger plist must have even length: ~S" value))
         (let ((alist '()))
           (loop for (key detail) on value by #'cddr
                 for name = (%event-name (if (and key (symbolp key))
                                             (string-downcase (symbol-name key))
                                             (%canvas-error "trigger event must be a symbol: ~S" key)))
                 do (unless (stringp detail)
                      (%canvas-error "trigger ~A: detail must be a string: ~S" name detail))
                    (let ((cell (assoc name alist :test #'string=)))
                      (if cell
                          (setf (cdr cell) detail)
                          (setf alist (append alist (list (cons name detail)))))))
           (stack-json:encode alist)))
        ((consp value)
         (format nil "~{~A~^, ~}" (mapcar #'%trigger-header-name value)))
        (t (%canvas-error "invalid trigger ~S" value))))

(defun %trigger-header-name (name)
  (unless (stringp name)
    (%canvas-error "trigger event must be a string: ~S" name))
  (%event-name name))

(defun htmx-response-headers (&key location push-url redirect refresh replace-url
                                   reselect reswap retarget trigger
                                   trigger-after-settle trigger-after-swap)
  "HX-* response headers as an alist, for the codec to send. See the file header."
  (let ((headers
          (loop for (name key value convert)
                  in (list (list "HX-Location" :location location #'%header-value)
                           (list "HX-Push-Url" :push-url push-url #'%header-value)
                           (list "HX-Redirect" :redirect redirect #'%header-value)
                           (list "HX-Refresh" :refresh refresh #'%header-value)
                           (list "HX-Replace-Url" :replace-url replace-url #'%header-value)
                           (list "HX-Reselect" :reselect reselect #'%header-value)
                           (list "HX-Reswap" :reswap reswap
                                 (lambda (k v) (declare (ignore k)) (hx-swap-value v)))
                           (list "HX-Retarget" :retarget retarget #'%header-value)
                           (list "HX-Trigger" :trigger trigger
                                 (lambda (k v) (declare (ignore k)) (%trigger-header v)))
                           (list "HX-Trigger-After-Settle" :trigger-after-settle trigger-after-settle
                                 (lambda (k v) (declare (ignore k)) (%trigger-header v)))
                           (list "HX-Trigger-After-Swap" :trigger-after-swap trigger-after-swap
                                 (lambda (k v) (declare (ignore k)) (%trigger-header v))))
                when value
                  collect (cons name (%no-crlf name (funcall convert key value))))))
    (stable-sort headers #'string< :key #'car)))

(defun sse-connect (link)
  (let ((url (link-url link)))
    (when url
      (list :hx-ext "sse" :sse-connect url))))

(defun sse-swap (event &key target swap)
  (append (list :sse-swap (%event-name event))
          (when target (list :hx-target (%string-option :target target)))
          (when swap (list :hx-swap (hx-swap-value swap)))))

(defun sse-close (event)
  (list :sse-close (%event-name event)))

(defun fragment-event (event tree &key id)
  "An sse-protocol event carrying TREE as an HTML fragment for sse-swap=EVENT."
  (%event-name event)
  (when id (%event-name id))
  (stack-sse:make-sse-event :event event :data (to-html tree) :id id))
