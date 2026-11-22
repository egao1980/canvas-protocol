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
;;; - VALS: a param plist (normalized as in resolver.lisp) -> a JSON object
;;;   string, keys in order, values strings; in strings escape " \ and
;;;   U+0000-U+001F (\n \r \t short forms, others \u00XX) and "<" as <.
;;; - PUSH-URL: T -> "true", a string as is.
;;;
;;; HTMX-RESPONSE-HEADERS -> alist ("HX-..." . string) sorted by header name
;;; (string<). Keys -> headers: :location HX-Location, :push-url HX-Push-Url,
;;; :redirect HX-Redirect, :refresh HX-Refresh (T -> "true"), :replace-url
;;; HX-Replace-Url, :reselect HX-Reselect, :reswap HX-Reswap (swap rules),
;;; :retarget HX-Retarget, :trigger HX-Trigger, :trigger-after-settle
;;; HX-Trigger-After-Settle, :trigger-after-swap HX-Trigger-After-Swap.
;;; Trigger values: an event name; a list of names -> "a, b"; a plist
;;; (:event "detail" ...) -> JSON object (VALS encoding). Event names follow the
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

(defun hx-swap-value (swap)
  "SWAP keyword or string -> the hx-swap attribute string. Invalid -> CANVAS-ERROR."
  (declare (ignore swap))
  (%h4h0-pending 'hx-swap-value))

(defun hx-get (link &key target swap trigger select include vals push-url confirm indicator)
  "Attribute plist for an hx-get to LINK. See the file header."
  (declare (ignore link target swap trigger select include vals push-url confirm indicator))
  (%h4h0-pending 'hx-get))

(defun hx-post (link &key target swap trigger select include vals push-url confirm indicator)
  "Attribute plist for an hx-post to LINK. See the file header."
  (declare (ignore link target swap trigger select include vals push-url confirm indicator))
  (%h4h0-pending 'hx-post))

(defun htmx-response-headers (&key location push-url redirect refresh replace-url
                                   reselect reswap retarget trigger
                                   trigger-after-settle trigger-after-swap)
  "HX-* response headers as an alist, for the codec to send. See the file header."
  (declare (ignore location push-url redirect refresh replace-url reselect reswap
                   retarget trigger trigger-after-settle trigger-after-swap))
  (%h4h0-pending 'htmx-response-headers))

(defun sse-connect (link)
  (declare (ignore link))
  (%h4h0-pending 'sse-connect))

(defun sse-swap (event &key target swap)
  (declare (ignore event target swap))
  (%h4h0-pending 'sse-swap))

(defun sse-close (event)
  (declare (ignore event))
  (%h4h0-pending 'sse-close))

(defun fragment-event (event tree &key id)
  "An sse-protocol event carrying TREE as an HTML fragment for sse-swap=EVENT."
  (declare (ignore event tree id))
  (%h4h0-pending 'fragment-event))
