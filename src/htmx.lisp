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

(defun %hx (verb link target swap trigger select include vals push-url confirm indicator)
  "Shared body of HX-GET / HX-POST: options are validated first, then a NIL URL -> NIL."
  (let* ((url (%hx-url verb link))
         (pairs (list (cons :hx-target (and target (%string-option :target target)))
                      (cons :hx-swap (and swap (hx-swap-value swap)))
                      (cons :hx-trigger (and trigger (%hx-trigger trigger)))
                      (cons :hx-select (and select (%string-option :select select)))
                      (cons :hx-include (and include (%string-option :include include)))
                      (cons :hx-vals (and vals (%hx-vals vals)))
                      (cons :hx-push-url (and push-url (%hx-push-url push-url)))
                      (cons :hx-confirm (and confirm (%string-option :confirm confirm)))
                      (cons :hx-indicator (and indicator (%string-option :indicator indicator))))))
    (when url
      (list* verb url
             (loop for (key . value) in pairs
                   when value nconc (list key value))))))

(defun hx-get (link &key target swap trigger select include vals push-url confirm indicator)
  "Attribute plist for an hx-get to LINK. See the file header."
  (%hx :hx-get link target swap trigger select include vals push-url confirm indicator))

(defun hx-post (link &key target swap trigger select include vals push-url confirm indicator)
  "Attribute plist for an hx-post to LINK. See the file header."
  (%hx :hx-post link target swap trigger select include vals push-url confirm indicator))

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
