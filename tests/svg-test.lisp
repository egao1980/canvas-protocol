(in-package #:canvas-protocol/tests)

;;; HX2 (0.2.0): SVG-FRAGMENT, engine-generated SVG through the allowlist.
;;; Rules: the header of src/svg.lisp.

(defparameter +svg-open+
  "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">")

(defun svg* (&rest inner)
  "An SVG document string with INNER (strings) as the root's content."
  (apply #'s+ +svg-open+ (append inner (list "</svg>"))))

(defun svg-html (source &rest keys)
  (to-html (apply #'svg-fragment source keys)))

(defun svg-violations (source)
  "The violation plists SVG-FRAGMENT reports for SOURCE, or :none."
  (handler-case (progn (svg-fragment source) :none)
    (unsafe-svg (c) (csp-error-violations c))))

(defun svg-stripped (source)
  "TO-HTML of SVG-FRAGMENT with every UNSAFE-SVG answered by STRIP-VIOLATIONS."
  (to-html (handler-bind ((unsafe-svg (lambda (c)
                                        (invoke-restart (find-restart 'strip-violations c)))))
             (svg-fragment source))))

(defun v (kind element &optional attribute)
  (list :kind kind :element element :attribute attribute))

(defparameter +small-svg+
  (s+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" (string #\Newline)
      "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\""
      " version=\"1.1\" viewBox=\"0 0 100 40\" preserveAspectRatio=\"xMidYMid meet\" width=\"100mm\">"
      "<defs><symbol id=\"g1\" viewBox=\"0 0 10 10\"><path d=\"M0 0L10 10\"/></symbol>"
      "<linearGradient id=\"lg\" gradientUnits=\"userSpaceOnUse\"><stop offset=\"0\" stop-color=\"#000\"/></linearGradient>"
      "<clipPath id=\"c1\" clipPathUnits=\"userSpaceOnUse\"><rect width=\"5\" height=\"5\"/></clipPath></defs>"
      "<g id=\"m1\" class=\"measure\" transform=\"translate(10,0)\" clip-path=\"url(#c1)\">"
      "<use xlink:href=\"#g1\" x=\"1\" y=\"2\"/><use href=\"#g1\" x=\"3\"/>"
      "<text x=\"0\" y=\"30\" font-size=\"8\" xml:space=\"preserve\"><tspan fill=\"url(#lg)\">A &amp; B &lt;</tspan></text>"
      "</g></svg>"))

(defparameter +small-html+
  (s+ "<svg viewBox=\"0 0 100 40\" preserveAspectRatio=\"xMidYMid meet\" width=\"100mm\">"
      "<defs><symbol id=\"g1\" viewBox=\"0 0 10 10\"><path d=\"M0 0L10 10\"></path></symbol>"
      "<linearGradient id=\"lg\" gradientUnits=\"userSpaceOnUse\"><stop offset=\"0\" stop-color=\"#000\"></stop></linearGradient>"
      "<clipPath id=\"c1\" clipPathUnits=\"userSpaceOnUse\"><rect width=\"5\" height=\"5\"></rect></clipPath></defs>"
      "<g id=\"m1\" class=\"measure\" transform=\"translate(10,0)\" clip-path=\"url(#c1)\">"
      "<use href=\"#g1\" x=\"1\" y=\"2\"></use><use href=\"#g1\" x=\"3\"></use>"
      "<text x=\"0\" y=\"30\" font-size=\"8\" xml:space=\"preserve\"><tspan fill=\"url(#lg)\">A &amp; B &lt;</tspan></text>"
      "</g></svg>"))

(deftest svg-fragment-golden
  (ok (string= +small-html+ (svg-html +small-svg+)))
  (let ((tree (svg-fragment +small-svg+)))
    (ok (stack-html:html-element-p tree))
    (ok (string= "svg" (stack-html:html-element-name tree)))
    (ok (eq tree (render tree)) "an html-protocol node renders as itself")
    (ok (equal '("svg" "defs" "symbol" "path" "linearGradient" "stop" "clipPath" "rect"
                 "g" "use" "use" "text" "tspan")
               (element-names tree))
        "camelCase element names kept")
    (ok (null (csp-violations tree)))))

(deftest svg-fragment-sources
  (let* ((doc (stack-xml:decode +small-svg+))
         (root (stack-xml:document-root-element doc)))
    (ok (string= +small-html+ (svg-html (stack-encoding:encode-to-octets +small-svg+ :encoding :utf-8)))
        "UTF-8 octets")
    (ok (string= +small-html+ (svg-html doc)) "an xml-protocol document")
    (ok (string= +small-html+ (svg-html root)) "an xml-protocol element")
    (ok (string= +small-html+ (svg-html root)) "the element is not modified"))
  (ok (string= "<svg><g id=\"a\"></g></svg>" (svg-html "<svg><g id=\"a\"/></svg>"))
      "an SVG without a namespace")
  (dolist (bad (list "<html xmlns=\"http://www.w3.org/1999/xhtml\"></html>"
                     "<svg xmlns=\"http://example.com/not-svg\"></svg>"
                     "<g xmlns=\"http://www.w3.org/2000/svg\"></g>"
                     42 '("<svg/>") (el :svg nil)))
    (ok (signals (svg-fragment bad) 'canvas-error) (format nil "~S" bad))))

(deftest svg-camel-case-and-ids
  (let ((out (svg-html (svg* "<defs><radialGradient id=\"rg\" gradientTransform=\"rotate(3)\" spreadMethod=\"pad\"/>"
                             "<pattern id=\"pt\" patternUnits=\"userSpaceOnUse\" patternContentUnits=\"objectBoundingBox\" patternTransform=\"scale(2)\"/>"
                             "<marker id=\"mk\" markerWidth=\"3\" markerHeight=\"3\" markerUnits=\"strokeWidth\" refX=\"1\" refY=\"2\" orient=\"auto\"/>"
                             "<mask id=\"mm\" maskUnits=\"userSpaceOnUse\" maskContentUnits=\"userSpaceOnUse\"/></defs>"
                             "<path id=\"p\" pathLength=\"10\" marker-end=\"url(#mk)\" mask=\"url(#mm)\" d=\"M0 0\"/>"
                             "<text id=\"t\" textLength=\"40\" lengthAdjust=\"spacing\"><textPath href=\"#p\" startOffset=\"5\">s</textPath></text>"))))
    (dolist (name '("radialGradient" "gradientTransform=" "spreadMethod=" "patternUnits="
                    "patternContentUnits=" "patternTransform=" "markerWidth=" "markerHeight="
                    "markerUnits=" "refX=" "refY=" "maskUnits=" "maskContentUnits=" "pathLength="
                    "textLength=" "lengthAdjust=" "<textPath" "startOffset="))
      (ok (contains name out) name))
    (dolist (id '("rg" "pt" "mk" "mm" "p" "t"))
      (ok (contains (s+ "id=\"" id "\"") out) id))))

(defparameter +rejection-rows+
  (list
   (list "script" "<script>alert(1)</script><g id=\"a\"/>"
         (list (v :inline-script "script")) "<svg><g id=\"a\"></g></svg>")
   (list "foreignObject" "<foreignObject><div xmlns=\"http://www.w3.org/1999/xhtml\">x</div></foreignObject>"
         (list (v :foreign-object "foreignObject")) "<svg></svg>")
   (list "on* any case" "<g onclick=\"x()\" onLoad=\"y()\" id=\"a\"/>"
         (list (v :event-handler "g" "onclick") (v :event-handler "g" "onLoad"))
         "<svg><g id=\"a\"></g></svg>")
   (list "external xlink:href" "<use xlink:href=\"https://example.com/s.svg#g\"/>"
         (list (v :external-href "use" "xlink:href")) "<svg><use></use></svg>")
   (list "data: href" "<use href=\"data:image/svg+xml;base64,AAAA\"/>"
         (list (v :external-href "use" "href")) "<svg><use></use></svg>")
   (list "javascript: href" "<use href=\"javascript:alert(1)\"/>"
         (list (v :external-href "use" "href")) "<svg><use></use></svg>")
   (list "bare #" "<use href=\"#\"/>"
         (list (v :external-href "use" "href")) "<svg><use></use></svg>")
   (list "# with a space" "<use href=\"#a b\"/>"
         (list (v :external-href "use" "href")) "<svg><use></use></svg>")
   (list "relative href" "<use href=\"glyphs.svg#a\"/>"
         (list (v :external-href "use" "href")) "<svg><use></use></svg>")
   (list "external url(" "<rect fill=\"url(https://example.com/p.svg#x)\" width=\"1\"/>"
         (list (v :css-url "rect" "fill")) "<svg><rect width=\"1\"></rect></svg>")
   (list "quoted url(" "<rect fill=\"url('#g')\"/>"
         (list (v :css-url "rect" "fill")) "<svg><rect></rect></svg>")
   (list "url( with fallback" "<rect stroke=\"url(#g) red\"/>"
         (list (v :css-url "rect" "stroke")) "<svg><rect></rect></svg>")
   (list "url( hidden in a value" "<text font-family=\"x url(//evil/f.woff)\">t</text>"
         (list (v :css-url "text" "font-family")) "<svg><text>t</text></svg>")
   (list "htmx attributes" "<g hx-post=\"/x\" data-hx-get=\"/y\" sse-connect=\"/s\" data-canvas-keys=\"ArrowUp\" HX-GET=\"/z\"/>"
         (list (v :htmx-attribute "g" "hx-post") (v :htmx-attribute "g" "data-hx-get")
               (v :htmx-attribute "g" "sse-connect") (v :htmx-attribute "g" "data-canvas-keys")
               (v :htmx-attribute "g" "HX-GET"))
         "<svg><g></g></svg>")
   (list "a link" "<a href=\"#x\"><path d=\"M0 0\"/></a><g/>"
         (list (v :element "a")) "<svg><g></g></svg>")
   (list "image" "<image href=\"#x\" width=\"1\"/>" (list (v :element "image")) "<svg></svg>")
   (list "animate" "<animate attributeName=\"href\" to=\"javascript:alert(1)\"/>"
         (list (v :element "animate")) "<svg></svg>")
   (list "set" "<set attributeName=\"fill\" to=\"red\"/>" (list (v :element "set")) "<svg></svg>")
   (list "filter subtree unexamined" "<filter id=\"f\"><feImage href=\"https://x/y.png\"/><script/></filter>"
         (list (v :element "filter")) "<svg></svg>")
   (list "XHTML element" "<html:div xmlns:html=\"http://www.w3.org/1999/xhtml\">x</html:div>"
         (list (v :element "div")) "<svg></svg>")
   (list "no-namespace element under an SVG root" "<g xmlns=\"\"><rect/></g>"
         (list (v :element "g")) "<svg></svg>")
   (list "unknown attributes" "<g tabindex=\"0\" focusable=\"true\" id=\"a\"/>"
         (list (v :attribute "g" "tabindex") (v :attribute "g" "focusable"))
         "<svg><g id=\"a\"></g></svg>")
   (list "filter attribute" "<g filter=\"url(#f)\" cursor=\"url(x.cur)\"/>"
         (list (v :attribute "g" "filter") (v :attribute "g" "cursor")) "<svg><g></g></svg>")
   (list "wrong case is another name" "<rect viewbox=\"0 0 1 1\" Fill=\"red\"/>"
         (list (v :attribute "rect" "viewbox") (v :attribute "rect" "Fill")) "<svg><rect></rect></svg>")
   (list "document order" "<g id=\"a\" onclick=\"x\"><script/><rect onmouseover=\"y\"/></g><use href=\"http://x\"/>"
         (list (v :event-handler "g" "onclick") (v :inline-script "script")
               (v :event-handler "rect" "onmouseover") (v :external-href "use" "href"))
         "<svg><g id=\"a\"><rect></rect></g><use></use></svg>"))
  "(name inner violations stripped-html): each a separate UNSAFE-SVG case.")

(deftest svg-rejections
  (loop for (name inner violations stripped) in +rejection-rows+
        for source = (svg* inner)
        do (ok (equal violations (svg-violations source)) name)
           (ok (string= stripped (svg-stripped source)) (s+ name " (stripped)"))))

(deftest svg-unsafe-condition
  (let ((c (handler-case (svg-fragment (svg* "<script/>")) (canvas-error (c) c))))
    (ok (typep c 'unsafe-svg))
    (ok (typep c 'canvas-csp-error) "an UNSAFE-SVG is a CANVAS-CSP-ERROR")
    (ok (stringp (canvas-error-message c))))
  (let ((names '()))
    (handler-case
        (handler-bind ((unsafe-svg (lambda (c)
                                     (setf names (mapcar #'restart-name (compute-restarts c))))))
          (svg-fragment (svg* "<script/>")))
      (unsafe-svg () nil))
    (ok (member 'strip-violations names))))

(deftest svg-silent-drops
  ;; Inert under the canvas CSP or in an HTML page: dropped without a violation.
  (dolist (row (list (list (svg* "<style>.a{fill:red}</style><g class=\"a\" style=\"fill:red\"/>")
                           "<svg><g class=\"a\"></g></svg>")
                     (list (svg* "<metadata><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"/></metadata><g/>")
                           "<svg><g></g></svg>")
                     (list (svg* "<!-- <script>x</script> --><?pi data?><g/>")
                           "<svg><g></g></svg>")
                     (list (svg* "<g xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\""
                                 " inkscape:label=\"Layer 1\" xml:base=\"http://x/\" xlink:title=\"t\" id=\"a\"/>")
                           "<svg><g id=\"a\"></g></svg>")
                     (list (s+ "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" baseProfile=\"full\""
                               " zoomAndPan=\"magnify\" contentScriptType=\"text/ecmascript\""
                               " contentStyleType=\"text/css\" enable-background=\"new 0 0 1 1\" width=\"1\"/>")
                           "<svg width=\"1\"></svg>")))
    (destructuring-bind (source expected) row
      (ok (eq :none (svg-violations source)) source)
      (ok (string= expected (svg-html source)) expected))))

(deftest svg-kept-specials
  (ok (string= (s+ "<svg role=\"img\" aria-label=\"Score\"><g id=\"n1\" data-note-id=\"n1\" aria-hidden=\"true\">"
                   "<text xml:lang=\"en\" xml:space=\"preserve\">a</text></g></svg>")
               (svg-html (s+ "<svg xmlns=\"http://www.w3.org/2000/svg\" role=\"img\" aria-label=\"Score\">"
                             "<g id=\"n1\" data-note-id=\"n1\" aria-hidden=\"true\">"
                             "<text xml:lang=\"en\" xml:space=\"preserve\">a</text></g></svg>"))))
  (ok (string= "<svg><use href=\"#a\"></use><use href=\"#a\"></use></svg>"
               (svg-html (svg* "<use href=\"#a\" xlink:href=\"#b\"/><use xlink:href=\"#b\" href=\"#a\"/>")))
      "href wins over xlink:href")
  (ok (equal (list (v :external-href "use" "xlink:href"))
             (svg-violations (svg* "<use href=\"#a\" xlink:href=\"http://x/b\"/>")))
      "both are checked")
  (ok (string= "<svg><rect fill=\"URL(#g)\" stroke=\" url(#g-2.x:y) \"></rect></svg>"
               (svg-html (svg* "<rect fill=\"URL(#g)\" stroke=\" url(#g-2.x:y) \"/>")))
      "url(#id) in any case, surrounding space allowed")
  (ok (string= "<svg><text>a&lt;b&gt;&amp;</text></svg>"
               (svg-html (svg* "<text>a<![CDATA[<b>&]]></text>")))
      "CDATA becomes escaped text"))

(deftest svg-control-characters
  ;; xml-backend-native >= 0.1.1 refuses a character reference to a control
  ;; character other than tab, LF and CR (XML 1.0 Legal Character): a parse
  ;; error, not wrapped. A tree built with xml-protocol can still hold such
  ;; characters, and they are removed.
  (ok (signals (svg-fragment (svg* "<g id=\"x&#7;y\"/>")) 'stack-xml:xml-well-formed-error))
  (ok (signals (svg-fragment (svg* "<text>a&#1;b</text>")) 'stack-xml:xml-well-formed-error))
  (ok (string= (format nil "<svg><g id=\"xy\"><text>ab~Cc~%d</text></g></svg>" #\Tab)
               (svg-html (svg* "<g id=\"xy\"><text>ab&#9;c&#10;d</text></g>")))
      "tab and LF references are kept")
  (ok (string= (format nil "<svg><g id=\"xy\"><text>ab~Cc~%d</text></g></svg>" #\Tab)
               (svg-html (stack-xml:make-xml-element
                          "svg" nil
                          (stack-xml:make-xml-element
                           "g" (list (cons "id" (format nil "x~Cy" (code-char 7))))
                           (stack-xml:make-xml-element
                            "text" nil (format nil "a~Cb~Cc~%d" (code-char 1) #\Tab))))))
      "controls in a built tree are removed from values and text"))

(deftest svg-attrs-option
  (ok (string= "<svg viewBox=\"0 0 10 10\" role=\"img\" aria-label=\"Bars 1-4\"></svg>"
               (to-html (svg-fragment (s+ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"210mm\""
                                          " height=\"297mm\" viewBox=\"0 0 10 10\" role=\"group\"/>")
                                      :attrs '(:width nil :height nil :role "img"
                                               :aria-label "Bars 1-4")))))
  (ok (signals (svg-fragment (svg*) :attrs '(:onclick "x()")) 'canvas-csp-error))
  (ok (signals (svg-fragment (svg*) :attrs '(:style "color:red")) 'canvas-csp-error))
  (ok (signals (svg-fragment (svg*) :attrs '(:width)) 'canvas-error)))

(deftest svg-xml-safety
  ;; xml-backend-native fetches nothing and expands no entity; SVG-FRAGMENT adds no handler.
  (ok (signals (svg-fragment (s+ "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
                                 " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
                                 +svg-open+ "</svg>"))
               'stack-xml:xml-entity-error)
      "external DTD")
  (ok (signals (svg-fragment (s+ "<!DOCTYPE svg [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]>"
                                 +svg-open+ "<text>&xxe;</text></svg>"))
               'stack-xml:xml-entity-error)
      "external entity")
  (ok (signals (svg-fragment (s+ "<!DOCTYPE svg [<!ENTITY a \"aaaaaaaa\"><!ENTITY b \"&a;&a;&a;&a;\">]>"
                                 +svg-open+ "<text>&b;</text></svg>"))
               'stack-xml:xml-entity-error)
      "internal entities are not expanded")
  (ok (signals (svg-fragment (s+ +svg-open+ "<g></svg>")) 'stack-xml:xml-parse-error)
      "not well-formed")
  (ok (string= "<svg><g id=\"a\"></g></svg>"
               (svg-html (s+ "<!DOCTYPE svg>" +svg-open+ "<g id=\"a\"/></svg>")))
      "a DOCTYPE without an external identifier is ignored"))

;;; A realistic engraver-style page: about 200 KB of glyph symbols used by
;;; reference, staff lines, nested groups with ids, classes and transforms,
;;; text, a gradient, a clip path and a <style> block (an engraver's embedded
;;; stylesheet, dropped: the page ships its own same-origin CSS).
;;; No time bound: xml-protocol 0.1.0's native parser is quadratic
;;; (xml-protocol#3, about 1.7 s for 94 KB), so the test parses the page only
;;; twice (once, and once more for the read-back) and stays correct when the
;;; parser gets faster.

(defparameter +engraver-shaped-svg+
  (s+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" (string #\Newline)
      "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\""
      " version=\"1.1\" width=\"2100px\" height=\"2970px\">"
      "<desc>Engraved by a test engraver</desc>"
      "<svg class=\"definition-scale\" color=\"black\" viewBox=\"0 0 21000 29700\">"
      "<style type=\"text/css\">g.page-margin{font-family:Times,serif;} g.ending, g.fing, g.reh, g.tempo{font-weight:bold;}</style>"
      "<defs><symbol id=\"E0A4-x1\" viewBox=\"0 0 1000 1000\" overflow=\"inherit\">"
      "<path transform=\"scale(1,-1)\" d=\"M0 -39c0 68 68 166 172 166\"/></symbol></defs>"
      "<g class=\"page-margin\" transform=\"translate(500, 500)\">"
      "<g id=\"m-1\" class=\"measure\" data-id=\"m-1\" data-class=\"measure\">"
      "<title class=\"labelAttr\">bar 1</title>"
      "<g id=\"s-1\" class=\"staff\"><path d=\"M0 0 L9000 0\" stroke=\"currentColor\" stroke-width=\"13\"/>"
      "<g id=\"n-1\" class=\"note\"><use xlink:href=\"#E0A4-x1\" x=\"600\" y=\"180\" height=\"720px\" width=\"720px\"/></g>"
      "<ellipse cx=\"1\" cy=\"2\" rx=\"3\" ry=\"4\"/><polygon points=\"0,0 1,1 2,0\"/>"
      "<polyline fill=\"none\" points=\"0,0 5,5\"/><rect x=\"0\" y=\"0\" height=\"9\" width=\"9\" rx=\"1\"/>"
      "<text x=\"0\" y=\"0\" font-size=\"0px\"><tspan class=\"text\" font-size=\"405px\">Allegro</tspan></text>"
      "</g></g></g></svg></svg>"))

(deftest svg-engraver-shape
  ;; Nested <svg> with the viewBox, the embedded <style> dropped, ids, classes,
  ;; data-*, titles and every shape kept, xlink:href -> href.
  (ok (string= (s+ "<svg width=\"2100px\" height=\"2970px\"><desc>Engraved by a test engraver</desc>"
                   "<svg class=\"definition-scale\" color=\"black\" viewBox=\"0 0 21000 29700\">"
                   "<defs><symbol id=\"E0A4-x1\" viewBox=\"0 0 1000 1000\" overflow=\"inherit\">"
                   "<path transform=\"scale(1,-1)\" d=\"M0 -39c0 68 68 166 172 166\"></path></symbol></defs>"
                   "<g class=\"page-margin\" transform=\"translate(500, 500)\">"
                   "<g id=\"m-1\" class=\"measure\" data-id=\"m-1\" data-class=\"measure\">"
                   "<title class=\"labelAttr\">bar 1</title>"
                   "<g id=\"s-1\" class=\"staff\"><path d=\"M0 0 L9000 0\" stroke=\"currentColor\" stroke-width=\"13\"></path>"
                   "<g id=\"n-1\" class=\"note\"><use href=\"#E0A4-x1\" x=\"600\" y=\"180\" height=\"720px\" width=\"720px\"></use></g>"
                   "<ellipse cx=\"1\" cy=\"2\" rx=\"3\" ry=\"4\"></ellipse><polygon points=\"0,0 1,1 2,0\"></polygon>"
                   "<polyline fill=\"none\" points=\"0,0 5,5\"></polyline><rect x=\"0\" y=\"0\" height=\"9\" width=\"9\" rx=\"1\"></rect>"
                   "<text x=\"0\" y=\"0\" font-size=\"0px\"><tspan class=\"text\" font-size=\"405px\">Allegro</tspan></text>"
                   "</g></g></g></svg></svg>")
               (svg-html +engraver-shaped-svg+)))
  (ok (eq :none (svg-violations +engraver-shaped-svg+)) "nothing to signal"))

(defparameter +glyph-ids+
  '("E050" "E062" "E0A2" "E0A3" "E0A4" "E1D5" "E240" "E241" "E260" "E262" "E4E3" "E4E5"))

(defun engraving-svg (&key (systems 6) (staves 4) (notes 30))
  (with-output-to-string (s)
    (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"210mm\" height=\"297mm\" viewBox=\"0 0 21000 29700\" preserveAspectRatio=\"xMidYMid meet\" overflow=\"visible\">~%")
    (format s "<desc>Synthetic engraving test page</desc>~%<defs>~%")
    (loop for glyph in +glyph-ids+
          for i from 0
          do (format s "<symbol id=\"glyph-~A\" viewBox=\"0 0 1000 1000\" overflow=\"inherit\"><path transform=\"scale(1,-1)\" d=\"M~D 0C~D 120 ~D 240 ~D 360L~D 480C~D 600 ~D 720 ~D 840Z\"/></symbol>~%"
                     glyph (* i 7) (+ 100 i) (+ 200 i) (+ 300 i) (+ 400 i) (+ 500 i) (+ 600 i) (+ 700 i)))
    (format s "<linearGradient id=\"hl\" gradientUnits=\"userSpaceOnUse\" x1=\"0\" y1=\"0\" x2=\"0\" y2=\"1\"><stop offset=\"0\" stop-color=\"#ffd\"/><stop offset=\"1\" stop-color=\"#fff\"/></linearGradient>~%")
    (format s "<clipPath id=\"page-clip\" clipPathUnits=\"userSpaceOnUse\"><rect x=\"0\" y=\"0\" width=\"21000\" height=\"29700\"/></clipPath>~%")
    (format s "</defs>~%<style type=\"text/css\">g.page-margin{font-family:Times,serif;} g.highlighted{fill:#c00;}</style>~%")
    (format s "<g id=\"page-1\" class=\"page-margin\" transform=\"translate(500, 500)\" clip-path=\"url(#page-clip)\">~%")
    (dotimes (sys systems)
      (format s "<g id=\"system-~D\" class=\"system\" transform=\"translate(0, ~D)\">~%" sys (* sys 4500))
      (dotimes (st staves)
        (format s "<g id=\"staff-~D-~D\" class=\"staff\" data-n=\"~D\">~%" sys st (1+ st))
        (dotimes (line 5)
          (let ((y (+ (* st 1000) (* line 180))))
            (format s "<path d=\"M0 ~D L20000 ~D\" stroke=\"currentColor\" stroke-width=\"13\"/>~%" y y)))
        (format s "<g id=\"layer-~D-~D\" class=\"layer\">~%" sys st)
        (dotimes (n notes)
          (let* ((id (format nil "note-~D-~D-~D" sys st n))
                 (x (+ 900 (* n 640)))
                 (y (+ (* st 1000) (* (mod (* n 7) 9) 90)))
                 (glyph (nth (mod (+ n st) (length +glyph-ids+)) +glyph-ids+))
                 (ref (if (zerop (mod n 3)) "href" "xlink:href")))
            (format s "<g id=\"~A\" class=\"note~:[~; highlighted~]\"~:[~; fill=\"url(#hl)\"~] aria-label=\"note ~D\"><use ~A=\"#glyph-~A\" x=\"~D\" y=\"~D\" height=\"720px\" width=\"720px\"/><g id=\"~A-stem\" class=\"stem\"><path d=\"M~D ~D L~D ~D\" stroke=\"currentColor\" stroke-width=\"18\"/></g></g>~%"
                    id (= n 5) (= n 5) n ref glyph x y id (+ x 290) y (+ x 290) (- y 630))))
        (format s "<text x=\"0\" y=\"~D\" font-size=\"405px\" class=\"label\"><tspan id=\"label-~D-~D\" font-family=\"Times,serif\" xml:space=\"preserve\">Part ~D &amp; voice &lt;~D&gt;</tspan></text>~%"
                (+ (* st 1000) 400) sys st (1+ st) (1+ st))
        (format s "</g>~%</g>~%"))
      (format s "</g>~%"))
    (format s "</g>~%</svg>~%")))

(defun attribute-values (text name)
  "Every NAME=\"value\" value in TEXT (a test helper over generated text)."
  (let ((needle (s+ " " name "=\"")) (out '()))
    (loop for start = (search needle text) then (search needle text :start2 (1+ start))
          while start
          do (let* ((b (+ start (length needle))) (e (position #\" text :start b)))
               (push (subseq text b e) out)))
    (nreverse out)))

(defun occurrences (needle text)
  (loop for start = (search needle text) then (search needle text :start2 (1+ start))
        while start count t))

(deftest svg-engraving-fixture
  (let ((svg (engraving-svg)))
    (ok (<= 190000 (length svg) 260000) (format nil "~D characters" (length svg)))
    (ok (= 720 (occurrences "<use " svg)))))

(defvar *engraving-tree* nil "The SVG-FRAGMENT of the engraving page, parsed once.")

(defun engraving-canvas () (card *engraving-tree* :id "page"))

(deftest svg-engraving-page
  (let* ((svg (engraving-svg))
         (tree (svg-fragment svg))
         (*engraving-tree* tree)
         (out (to-html tree)))
    (ok (equal (attribute-values svg "id") (attribute-values out "id")) "every id kept, in order")
    (ok (= 720 (occurrences "<use " out)))
    (ok (= (occurrences "<g " svg) (occurrences "<g " out)) "every group kept")
    (ok (= (occurrences "<path " svg) (occurrences "<path " out)) "every path kept")
    (ok (every (lambda (h) (char= #\# (char h 0))) (attribute-values out "href")))
    (ok (= 720 (length (attribute-values out "href"))) "xlink:href and href both become href")
    (dolist (s '("viewBox=\"0 0 21000 29700\"" "preserveAspectRatio=\"xMidYMid meet\"" "<clipPath"
                 "clipPathUnits=" "<linearGradient" "gradientUnits=" "clip-path=\"url(#page-clip)\""
                 "fill=\"url(#hl)\"" "xml:space=\"preserve\"" "Part 1 &amp; voice &lt;1&gt;"))
      (ok (contains s out) s))
    (dolist (s '("<style" "version=" "xmlns" "xlink:" "<?xml" "type=\"text/css\""))
      (ok (not (contains s out)) s))
    (ok (null (csp-violations tree)))
    (ok (string= out (to-html (neutralize tree))) "neutralize keeps it unchanged")
    (ok (string= out (svg-html out)) "its HTML reads back to the same tree")
    (let ((page (with-fake () (to-html (page tree :title "Score")))))
      (ok (contains out page)))
    (let ((frozen (freeze 'engraving-canvas)))
      (ok (contains out frozen) "freeze keeps the SVG"))))
