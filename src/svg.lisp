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
;;; A character reference outside XML 1.0 Char (&#1;, &#7;, ...) signals
;;; STACK-XML:XML-WELL-FORMED-ERROR (xml-backend-native >= 0.1.1).
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

;;; Implementation. One parse, then one walk that builds the clean tree and
;;; collects the violations together; STRIP-VIOLATIONS returns that tree. The
;;; allow/deny decisions are made in exactly two places, %SVG-ELEMENT-VERDICT
;;; and %SVG-ATTRIBUTE-VERDICT, both driven by the tables above; the walker
;;; only carries them out. Nothing from the input is interned, and output
;;; names come only from the tables, "href", "xml:space" / "xml:lang", or the
;;; aria-* / data-* patterns.

(defparameter +xml-namespace+ "http://www.w3.org/XML/1998/namespace")
(defparameter +xmlns-namespace+ "http://www.w3.org/2000/xmlns/")

(defparameter +svg-htmx-prefixes+ '("hx-" "data-hx-" "sse-" "data-sse-" "data-canvas-")
  "No-namespace attribute prefixes (any case) htmx or canvas-keys.js would act on.")

(defparameter +svg-kept-controls+ (coerce '(#\Tab #\Newline #\Return) 'string)
  "The control characters kept in SVG text and attribute values.")

(defun %svg-ns (uri)
  "URI with \"\" folded to NIL: both mean no namespace."
  (and uri (plusp (length uri)) uri))

(defun %white-space-char-p (char)
  "CHAR is Unicode White_Space."
  (let ((code (char-code char)))
    (or (<= 9 code 13) (= code 32) (= code 133) (= code 160) (= code 5760)
        (<= 8192 code 8202) (= code 8232) (= code 8233) (= code 8239) (= code 8287)
        (= code 12288))))

(defun %svg-fragment-ref-p (value &optional (start 0) (end (length value)))
  "The HREF rule on VALUE[START,END): \"#\" followed by one or more characters,
none of them whitespace or a control character."
  (and (> (- end start) 1)
       (char= (char value start) #\#)
       (loop for i from (1+ start) below end
             never (let ((c (char value i)))
                     (or (%control-char-p c) (%white-space-char-p c))))))

(defun %svg-url-ok-p (value)
  "The URL rule: VALUE has no \"url(\" (any case), or VALUE trimmed of space, tab,
CR and LF is url(#ID), ID as in the HREF rule and without \")\"."
  (or (null (search "url(" value :test #'char-equal))
      (let* ((v (string-trim '(#\Space #\Tab #\Return #\Newline) value))
             (close (1- (length v))))
        (and (> close 4)
             (%prefix-p "url(" v)
             (char= (char v close) #\))
             (%svg-fragment-ref-p v 4 close)
             (not (find #\) v :start 4 :end close))))))

(defun %svg-name-p (name prefix rest-ok)
  "NAME is PREFIX (case-sensitive) followed by one or more characters satisfying REST-OK."
  (let ((n (length prefix)))
    (and (> (length name) n)
         (string= prefix name :end2 n)
         (loop for i from n below (length name)
               always (funcall rest-ok (char name i))))))

(defun %svg-data-char-p (char)
  (or (%ascii-alnum-p char) (char= char #\-)))

(defun %svg-element-verdict (element root-ns)
  "The allowlist's decision on the xml-element ELEMENT below a root in ROOT-NS:
:keep, :drop (with its subtree, no violation) or a violation kind. The
namespace is checked before the local name; names compare case-sensitively."
  (let ((name (stack-xml:xml-element-local-name element)))
    (cond ((not (equal (%svg-ns (stack-xml:xml-element-namespace-uri element)) root-ns)) :element)
          ((member name +svg-elements+ :test #'string=) :keep)
          ((member name +svg-dropped-elements+ :test #'string=) :drop)
          ((string= name "script") :inline-script)
          ((string= name "foreignObject") :foreign-object)
          (t :element))))

(defun %svg-plain-href-p (attribute)
  "ATTRIBUTE is a no-namespace href."
  (and (null (%svg-ns (stack-xml:xml-attribute-namespace-uri attribute)))
       (string= (stack-xml:xml-qname attribute) "href")))

(defun %svg-attribute-verdict (attribute plain-href-p)
  "The allowlist's decision on ATTRIBUTE (an xml-attribute of a kept element):
(values :keep OUTPUT-NAME OUTPUT-VALUE), :drop (no violation) or a violation
kind. PLAIN-HREF-P: the element also has a no-namespace href, which wins over
xlink:href. The HREF rule sees the value as parsed; the URL rule sees it both
as parsed and as kept (control characters removed)."
  (let ((ns (%svg-ns (stack-xml:xml-attribute-namespace-uri attribute)))
        (prefix (stack-xml:xml-attribute-prefix attribute))
        (local (stack-xml:xml-attribute-local-name attribute))
        (value (stack-xml:xml-attribute-value attribute)))
    (unless (stringp value)
      (%canvas-error "svg-fragment: the value of ~A must be a string, not ~S"
                     (stack-xml:xml-qname attribute) value))
    (flet ((keep (name)
             (let ((kept (%remove-controls value +svg-kept-controls+)))
               (if (and (%svg-url-ok-p value) (or (eq kept value) (%svg-url-ok-p kept)))
                   (values :keep name kept)
                   :css-url)))
           (href-ok-p () (%svg-fragment-ref-p value)))
      (cond ((or (equal ns +xmlns-namespace+) (equal prefix "xmlns")
                 (and (null prefix) (equal local "xmlns")))
             :drop)
            ((or (equal ns +xml-namespace+) (equal prefix "xml"))
             (if (member local '("space" "lang") :test #'string=)
                 (keep (%str "xml:" local))
                 :drop))
            ((equal ns +xlink-namespace+)
             (cond ((not (string= local "href")) :drop)
                   ((not (href-ok-p)) :external-href)
                   (plain-href-p :drop)  ; checked, then href wins (SVG 2)
                   (t (keep "href"))))
            (ns :drop)
            (t (let ((name (stack-xml:xml-qname attribute)))
                 (cond ((%prefix-p "on" name) :event-handler)
                       ((some (lambda (p) (%prefix-p p name)) +svg-htmx-prefixes+) :htmx-attribute)
                       ((member name +svg-dropped-attributes+ :test #'string=) :drop)
                       ((string= name "href") (if (href-ok-p) (keep "href") :external-href))
                       ((or (member name +svg-attributes+ :test #'string=)
                            (%svg-name-p name "aria-" #'%ascii-lower-p)
                            (%svg-name-p name "data-" #'%svg-data-char-p))
                        (keep name))
                       (t :attribute))))))))

(defun %svg-text (string)
  (%text (%remove-controls string +svg-kept-controls+)))

(defun %svg-attrs (attrs)
  "ATTRS with EL's rules -> list of (name . string-or-NIL); NIL means remove.
Not %ATTR-ALIST, which drops NIL values."
  (unless (%plist-p attrs)
    (%canvas-error "svg-fragment :attrs must be a plist: ~S" attrs))
  (loop for (key value) on attrs by #'cddr
        for name = (%attr-name key)
        for string = (%attr-value name value)
        do (let ((kind (and string (%attribute-violation name string))))
             (when kind (%csp-error kind "svg" name)))
        collect (cons name string)))

(defun %svg-apply-attrs (pairs attrs)
  "PAIRS (fresh conses) with ATTRS applied: an existing name gets the new value in
place, a new name is appended, a NIL value removes the attribute."
  (dolist (attr attrs pairs)
    (destructuring-bind (name . value) attr
      (let ((cell (assoc name pairs :test #'string-equal)))
        (cond ((null value) (setf pairs (remove name pairs :key #'car :test #'string-equal)))
              (cell (setf (cdr cell) value))
              (t (setf pairs (append pairs (list (cons name value))))))))))

(defun %svg-convert (root attrs)
  "ROOT (the checked <svg> xml-element) -> (values html-element violations), in one
walk in document order. ATTRS (from %SVG-ATTRS) are applied to the root."
  (let ((root-ns (%svg-ns (stack-xml:xml-element-namespace-uri root)))
        (found '()))
    (labels ((violation (kind element attribute)
               (push (list :kind kind :element element :attribute attribute) found))
             (kept-attributes (node element)
               (let* ((attributes (stack-xml:xml-element-attributes node))
                      (plain-href-p (some #'%svg-plain-href-p attributes))
                      (kept '()))
                 (dolist (a attributes (nreverse kept))
                   (multiple-value-bind (verdict name value) (%svg-attribute-verdict a plain-href-p)
                     (case verdict
                       (:keep (let ((name (stack-html:canonical-attribute-name name)))
                                ;; Equal after canonicalization (data-a, data-A): the
                                ;; first wins, so the output stays well-formed.
                                (unless (assoc name kept :test #'string=)
                                  (push (cons name value) kept))))
                       (:drop)
                       (t (violation verdict element (stack-xml:xml-qname a))))))))
             (element (node ops)
               (let ((name (stack-xml:xml-element-local-name node)))
                 ;; Arguments evaluate left to right: attributes before children.
                 (stack-html:make-html-element
                  name
                  :attributes (%svg-apply-attrs (kept-attributes node name) ops)
                  :children (children node))))
             (child (node)
               (typecase node
                 (stack-xml:xml-element
                  (let ((verdict (%svg-element-verdict node root-ns)))
                    (case verdict
                      (:keep (element node nil))
                      (:drop nil)
                      (t (violation verdict (stack-xml:xml-element-local-name node) nil)
                         nil))))          ; the subtree is not examined
                 (stack-xml:xml-text (%svg-text (stack-xml:xml-text-data node)))
                 (stack-xml:xml-cdata (%svg-text (stack-xml:xml-cdata-data node)))
                 ((or stack-xml:xml-comment stack-xml:xml-pi) nil)
                 (t (%canvas-error "svg-fragment: unsupported XML node ~(~A~)" (type-of node)))))
             (children (node)
               (loop for c in (stack-xml:xml-element-children node)
                     for out = (child c)
                     when out collect out)))
      (let ((tree (element root attrs)))
        (values tree (nreverse found))))))

(defun %svg-root (source)
  "SOURCE -> its <svg> root xml-element, checked. Strings and octets are parsed
with no handler around STACK-XML:DECODE."
  (let ((root (typecase source
                ((or string (vector (unsigned-byte 8)))
                 (stack-xml:document-root-element (stack-xml:decode source)))
                (stack-xml:xml-document (stack-xml:document-root-element source))
                (stack-xml:xml-element source)
                (t (%canvas-error "svg-fragment needs SVG text, UTF-8 octets, an xml-document or an xml-element, not a ~(~A~)"
                                  (type-of source))))))
    (unless (and (stack-xml:xml-element-p root)
                 (equal (stack-xml:xml-element-local-name root) "svg")
                 (member (%svg-ns (stack-xml:xml-element-namespace-uri root))
                         (list nil +svg-namespace+) :test #'equal))
      (%canvas-error "svg-fragment: the root must be an <svg> element in the SVG namespace or in none"))
    root))

(defun svg-fragment (source &key attrs)
  "SOURCE (SVG text, UTF-8 octets, or an xml-protocol document / element) ->
an html-protocol <svg> element through the allowlist. See the file header."
  (let* ((attrs (%svg-attrs attrs))     ; before the parse, the expensive step
         (root (%svg-root source)))
    (multiple-value-bind (tree violations) (%svg-convert root attrs)
      (if violations
          (restart-case
              (error 'unsafe-svg
                     :violations violations
                     :message (destructuring-bind (&key kind element attribute) (first violations)
                                (format nil "~D SVG allowlist violation~:P, the first ~(~A~) in <~A>~@[ attribute ~A~]"
                                        (length violations) kind element attribute)))
            (strip-violations ()
              :report "Drop the offending SVG elements and attributes and go on."
              tree))
          tree))))
