(in-package #:canvas-protocol)

;;; Page, CSP scan, neutralize, freeze.
;;;
;;; PAGE body &key (title "canvas") (lang "en") (stylesheet :link) (htmx t)
;;; -> html-document:
;;;   <!DOCTYPE html><html lang="LANG"><head><meta charset="utf-8">
;;;   <meta name="viewport" content="width=device-width, initial-scale=1">
;;;   [<meta name="htmx-config" content="JSON of +HTMX-CONFIG+">]       (htmx t)
;;;   <title>TITLE</title>
;;;   [<link rel="stylesheet" href="(resolve-asset-url r "canvas.css")">] (:link)
;;;   [<style>(canvas-stylesheet)</style>]                                (:inline)
;;;   [<script src="...htmx.min.js"></script><script src="...htmx-ext-sse.min.js"></script>] (htmx t)
;;;   </head><body>BODY</body></html>
;;; STYLESHEET :link, :inline or NIL. Asset URLs from *CANVAS-RESOLVER*
;;; (NIL resolver -> CANVAS-NO-RESOLVER). The rendered BODY is scanned first:
;;; any CSP-VIOLATIONS -> CANVAS-CSP-ERROR (violations) with restart
;;; STRIP-VIOLATIONS (remove offending elements / attributes and go on).
;;;
;;; +HTMX-CONFIG+ is an alist in output order in json-protocol's value mapping
;;; (T -> true, NIL -> false, integers). The meta content is
;;; (stack-json:encode +htmx-config+); no JSON encoder of our own (key order:
;;; egao1980/json-protocol#23).
;;;
;;; CSP-VIOLATIONS tree -> list, in document order (TREE is RENDERed first; a
;;; document is scanned whole), of
;;;   (:kind KIND :element "tag" :attribute "name-or-NIL")
;;; KIND: :inline-script (a <script> with text content or without src),
;;; :embed (<iframe> <object> <embed>), :event-handler (on* attribute),
;;; :hx-on (hx-on* / data-hx-on* attribute), :javascript-url (a URL attribute
;;; starting javascript:), :inline-style (style attribute), :eval-trigger
;;; (hx-trigger / data-hx-trigger containing "["), :js-vals (hx-vals starting
;;; "js:", or hx-vars). A <script src> with no content is fine.
;;;
;;; NEUTRALIZE tree -> a fresh tree (the input is not modified) without:
;;; hx-* / data-hx-* / sse-* attributes; <script> <link> <iframe> <object>
;;; <embed> <style> elements; on* and style attributes; src href action
;;; formaction srcset poster xlink:href values unless they start "#" or "data:".
;;;
;;; FREEZE fn &key args title -> string. FN an fbound symbol or a function
;;; (else CANVAS-ERROR); ARGS a plist (odd -> CANVAS-ERROR). Calls FN with ARGS
;;; under a FREEZE-RESOLVER, renders, NEUTRALIZEs, and returns
;;;   <!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
;;;   <meta name="viewport" content="width=device-width, initial-scale=1">
;;;   <title>TITLE</title><style>(canvas-stylesheet)</style></head>
;;;   <body data-canvas-frozen="">...</body></html>
;;; TITLE default: the lowercased symbol name, or "canvas" for a function object.
;;; htmx is dropped, not disabled (decision: a frozen file has no server).
;;; Byte-deterministic for the same function, args and image state.

(defparameter +htmx-config+
  '(("allowEval" . nil)
    ("allowScriptTags" . nil)
    ("historyCacheSize" . 0)
    ("includeIndicatorStyles" . nil)
    ("refreshOnHistoryMiss" . t)
    ("selfRequestsOnly" . t))
  "htmx.config for CSP (no eval, no inline styles) and privacy (no history snapshots).")

(defun page (body &key (title "canvas") (lang "en") (stylesheet :link) (htmx t))
  (declare (ignore body title lang stylesheet htmx))
  (%h4h0-pending 'page))

(defun csp-violations (tree)
  (declare (ignore tree))
  (%h4h0-pending 'csp-violations))

(defun neutralize (tree)
  (declare (ignore tree))
  (%h4h0-pending 'neutralize))

(defun freeze (fn &key args title)
  (declare (ignore fn args title))
  (%h4h0-pending 'freeze))
