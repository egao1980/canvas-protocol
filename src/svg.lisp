(in-package #:canvas-protocol)

;;; Engine-generated SVG (0.2.0, HX2). A third-party renderer (an engraver, a
;;; diagram layout engine) hands over SVG text. SVG-FRAGMENT turns it into an
;;; html-protocol tree through an allowlist, so RENDER, CSP-VIOLATIONS,
;;; NEUTRALIZE and FREEZE treat it like any other tree. There is no raw-string
;;; escape hatch: nothing reaches the page without passing these rules.
;;;
;;; SVG-FRAGMENT source &key attrs -> one html-protocol element named "svg".
;;;
;;; SOURCE
;;; - a string or an (unsigned-byte 8) vector (UTF-8): parsed with
;;;   (STACK-XML:DECODE source) under the current STACK-XML:*XML-BACKEND*
;;;   (xml-backend-native, a dependency, installs itself when loaded);
;;; - a STACK-XML:XML-DOCUMENT: its root element (DOCUMENT-ROOT-ELEMENT);
;;; - a STACK-XML:XML-ELEMENT: used as is (never modified);
;;; - anything else -> CANVAS-ERROR.
;;; Parse errors are xml-protocol's own conditions (STACK-XML:XML-PARSE-ERROR
;;; and its subclasses), not wrapped. SVG-FRAGMENT establishes no handler and
;;; no restart around the parse. xml-backend-native fetches nothing: a
;;; DOCTYPE with an external identifier (PUBLIC or SYSTEM) and an external
;;; entity declaration signal STACK-XML:XML-ENTITY-ERROR (the backend's
;;; CONTINUE restart only records the identifiers), and a reference to any
;;; entity other than the five predefined ones signals XML-ENTITY-ERROR too: no
;;; entity is ever expanded, so there are no external entities and no
;;; expansion bombs. A DOCTYPE without an external identifier is ignored.
;;;
;;; ROOT: an element with local name "svg" in +SVG-NAMESPACE+ or in no
;;; namespace (a namespace URI of NIL or ""); otherwise CANVAS-ERROR. Every
;;; element below it must be in the root's namespace (NIL and "" count as the
;;; same); one in any other namespace (an XHTML <div>, an editor's private
;;; element, a no-namespace element under an SVG-namespace root) is a
;;; violation of kind :element.
;;;
;;; ELEMENTS, in document order:
;;; - local name in +SVG-ELEMENTS+ (compared case-sensitively, as XML names
;;;   are) -> kept, named by STACK-HTML:CANONICAL-ELEMENT-NAME, so clipPath,
;;;   linearGradient, textPath keep their case;
;;; - local name in +SVG-DROPPED-ELEMENTS+ ("style" "metadata") -> dropped
;;;   with its subtree, no violation. Decision: a <style> element has no effect
;;;   on a canvas page (the CSP's style-src 'self' ignores it, and NEUTRALIZE
;;;   removes it from frozen files), so dropping it changes nothing the viewer
;;;   sees; style engine output through class and the page stylesheet;
;;; - anything else -> a violation, and nothing inside it is examined:
;;;   "script" :inline-script, "foreignObject" :foreign-object, every other
;;;   name (a, image, animate, set, filter and fe*, iframe, switch, font, ...)
;;;   :element.
;;;
;;; ATTRIBUTES of a kept element, in source order:
;;; - namespace declarations (xmlns, xmlns:*) -> dropped, no violation;
;;; - XML namespace: xml:space and xml:lang kept, under those names; any other
;;;   (xml:base) dropped;
;;; - XLink namespace: xlink:href is checked by the HREF rule and kept under
;;;   the name "href" (SVG 2), so the result needs no xlink declaration; any
;;;   other xlink:* attribute dropped. An element with both href and xlink:href
;;;   keeps href and drops xlink:href (SVG 2 precedence); both are checked;
;;; - any other namespace (an editor's inkscape:*, sodipodi:*) -> dropped;
;;; - no namespace, first rule that applies:
;;;     name starts with "on", any case                 -> :event-handler
;;;     name starts with hx- data-hx- sse- data-sse-
;;;       or data-canvas-, any case (htmx would act on them) -> :htmx-attribute
;;;     in +SVG-DROPPED-ATTRIBUTES+ (style, version, ...) -> dropped, no
;;;       violation (style for the reason <style> is; the rest are inert)
;;;     "href"                                          -> the HREF rule
;;;     in +SVG-ATTRIBUTES+ (case-sensitive), or matching aria-[a-z]+ or
;;;       data-[A-Za-z0-9-]+                            -> kept
;;;     otherwise                                       -> :attribute
;;; - HREF rule: the value is a same-document reference, "#" followed by one or
;;;   more characters none of which is whitespace or a control character;
;;;   otherwise :external-href.
;;; - URL rule, for every value about to be kept: when it contains "url("
;;;   (any case), the whole value, trimmed of space, tab, CR and LF, must be
;;;   "url(#ID)" ("url(" in any case) with ID as in the HREF rule and without
;;;   ")"; otherwise :css-url. The value is kept as given. So
;;;   fill="url(#grad)", fill="URL(#grad)" and clip-path=" url(#c1) " stay;
;;;   fill="url(https://...)", "url('#a')" and "url(#a) red" do not.
;;; Kept attribute names go through STACK-HTML:CANONICAL-ATTRIBUTE-NAME
;;; (viewBox, preserveAspectRatio, gradientUnits, refX keep their case).
;;; Values are kept as given, except that control characters other than tab,
;;; LF and CR are removed.
;;;
;;; CHILDREN: elements as above; text and CDATA -> html-text, escaped on output
;;; by html-protocol, control characters other than tab, LF and CR removed;
;;; comments and processing instructions dropped (html-protocol writes comment
;;; data raw).
;;;
;;; VIOLATIONS: a list in document order of
;;;   (:kind KIND :element LOCAL-NAME :attribute QNAME-or-NIL)
;;; the CSP-VIOLATIONS shape; for an attribute violation :element is the
;;; element's local name, for an element violation :attribute is NIL. QNAME is
;;; the attribute's name as written in the source ("onclick", "xlink:href").
;;; A non-empty list -> UNSAFE-SVG (a CANVAS-CSP-ERROR; CSP-ERROR-VIOLATIONS
;;; is the list) signalled with ERROR inside the restart STRIP-VIOLATIONS,
;;; which returns the result without the offending elements (with their
;;; subtrees) and attributes.
;;;
;;; ATTRS: a plist with EL's rules (names, values, CSP refusals), applied to
;;; the root after the rules above: a name already on the root gets the new
;;; value in place, a new name is appended, and a NIL value removes the
;;; attribute (so (:width nil :height nil) leaves only viewBox for sizing).
;;;
;;; The result: an html-protocol element tree that CSP-VIOLATIONS finds no
;;; violation in and that NEUTRALIZE keeps unchanged (only "#" references).
;;; Its TO-HTML text is well-formed XML without namespace declarations, and
;;; SVG-FRAGMENT of that text has the same TO-HTML text again, provided no
;;; attribute value holds a tab, LF or CR (an XML parser may normalize those
;;; to spaces). Same input -> same tree.

(defparameter +svg-namespace+ "http://www.w3.org/2000/svg")
(defparameter +xlink-namespace+ "http://www.w3.org/1999/xlink")

(defparameter +svg-elements+
  '("svg" "g" "defs" "symbol" "use" "path" "rect" "circle" "ellipse" "line"
    "polyline" "polygon" "text" "tspan" "textPath" "title" "desc" "clipPath"
    "mask" "marker" "pattern" "linearGradient" "radialGradient" "stop")
  "SVG elements SVG-FRAGMENT keeps: structure, shapes, text, paint servers, clipping.")

(defparameter +svg-dropped-elements+ '("style" "metadata")
  "Elements SVG-FRAGMENT drops (with their subtrees) without a violation.")

(defparameter +svg-attributes+
  '(;; core
    "id" "class" "lang" "role"
    ;; geometry, text layout, coordinate systems
    "x" "y" "x1" "y1" "x2" "y2" "cx" "cy" "r" "rx" "ry" "fx" "fy" "fr"
    "width" "height" "d" "points" "pathLength" "dx" "dy" "rotate"
    "textLength" "lengthAdjust" "startOffset" "method" "spacing" "side" "offset"
    "viewBox" "preserveAspectRatio" "transform"
    "gradientTransform" "gradientUnits" "spreadMethod"
    "patternUnits" "patternContentUnits" "patternTransform"
    "clipPathUnits" "maskUnits" "maskContentUnits"
    "markerWidth" "markerHeight" "markerUnits" "refX" "refY" "orient"
    ;; presentation attributes
    "alignment-baseline" "baseline-shift" "clip" "clip-path" "clip-rule" "color"
    "color-interpolation" "color-rendering" "direction" "display"
    "dominant-baseline" "fill" "fill-opacity" "fill-rule" "font-family"
    "font-size" "font-size-adjust" "font-stretch" "font-style" "font-variant"
    "font-weight" "image-rendering" "letter-spacing" "marker-start" "marker-mid"
    "marker-end" "mask" "opacity" "overflow" "paint-order" "pointer-events"
    "shape-rendering" "stop-color" "stop-opacity" "stroke" "stroke-dasharray"
    "stroke-dashoffset" "stroke-linecap" "stroke-linejoin" "stroke-miterlimit"
    "stroke-opacity" "stroke-width" "text-anchor" "text-decoration"
    "text-rendering" "transform-origin" "unicode-bidi" "vector-effect"
    "visibility" "word-spacing" "writing-mode")
  "No-namespace attributes SVG-FRAGMENT keeps (besides href, aria-* and data-*),
spelled as SVG spells them.")

(defparameter +svg-dropped-attributes+
  '("style" "version" "baseProfile" "zoomAndPan" "contentScriptType"
    "contentStyleType" "enable-background")
  "No-namespace attributes SVG-FRAGMENT drops without a violation.")

(defun svg-fragment (source &key attrs)
  "SOURCE (SVG text, UTF-8 octets, or an xml-protocol document / element) ->
an html-protocol <svg> element through the allowlist. See the file header."
  (declare (ignorable source attrs))
  (%hx2-pending 'svg-fragment))
