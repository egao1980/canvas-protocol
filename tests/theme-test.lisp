(in-package #:canvas-protocol/tests)

;;; Chart series colours (issue #2) and SVG camelCase names (issue #3).

(defun %lines (string)
  (loop for start = 0 then (1+ end)
        for end = (position #\Newline string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(deftest chart-css-rules
  (let* ((css (chart-css))
         (lines (%lines css)))
    (ok (= 11 (length lines)) "theme rule, @media rule, 8 series rules, trailing newline")
    (ok (string= "" (car (last lines))))
    (ok (string= (s+ ":root{color-scheme:light dark;--chart-1:#E69F00;--chart-2:#56B4E9;"
                     "--chart-3:#009E73;--chart-4:#F0E442;--chart-5:#0072B2;--chart-6:#D55E00;"
                     "--chart-7:#CC79A7;--chart-8:#000000}")
                 (first lines)))
    (ok (string= (s+ "@media (prefers-color-scheme: dark){:root{--chart-1:#E69F00;--chart-2:#56B4E9;"
                     "--chart-3:#009E73;--chart-4:#F0E442;--chart-5:#0072B2;--chart-6:#D55E00;"
                     "--chart-7:#CC79A7;--chart-8:#E6EDF3}}")
                 (second lines)))
    ;; The selectors and fallbacks of canvas-backend-svg's PALETTE-CSS, 1-based.
    (loop for colour in *chart-palette*
          for n from 1
          for line in (cddr lines)
          do (ok (string= (format nil "figure[data-chart] [data-series=\"~D\"]:not([color]){color:var(--chart-~D, ~A)}"
                                  n n colour)
                          line)
                 (format nil "series ~D" n)))
    (ok (= (length *chart-palette*) (length *chart-palette-dark*)))
    (ok (not (string-equal "#000000" (car (last *chart-palette-dark*)))) "no black on dark")))

(deftest chart-css-options
  (ok (string= (s+ ":root{color-scheme:light dark;--chart-1:#000;--chart-2:#fff}" (string #\Newline)
                   "@media (prefers-color-scheme: dark){:root{--chart-1:#111;--chart-2:#eee}}" (string #\Newline)
                   "figure[data-chart] [data-series=\"1\"]:not([color]){color:var(--chart-1, #000)}" (string #\Newline)
                   "figure[data-chart] [data-series=\"2\"]:not([color]){color:var(--chart-2, #fff)}" (string #\Newline))
               (chart-css :light '("#000" "#fff") :dark '("#111" "#eee"))))
  (ok (typep (chart-stylesheet) 'stack-css:stylesheet))
  (ok (signals (chart-stylesheet :light '("#000") :dark '("#111" "#222")) 'canvas-error)))

(deftest placeholder-theme-carries-chart-css
  (let ((css (canvas-stylesheet))
        (chart (chart-css)))
    (ok (string= chart (subseq css (- (length css) (length chart))))
        "assets/canvas.css ends with (chart-css): regenerate it when the palettes change")
    (ok (contains "[data-series=\"8\"]" css))))

;;; SVG camelCase names survive EL, RENDER, TO-HTML and FREEZE.

(defun svg-canvas ()
  (el :figure '(:data-chart "line")
      (el :svg '(:viewbox "0 0 10 10" :preserveaspectratio "xMidYMid meet")
          (el :defs nil
              (el :lineargradient '(:id "g" :gradientunits "userSpaceOnUse")))
          (el :rect '(:data-series 1 :width 10 :height 10 :fill "currentColor")))))

(defparameter +svg-html+
  (s+ "<figure data-chart=\"line\"><svg viewBox=\"0 0 10 10\" preserveAspectRatio=\"xMidYMid meet\">"
      "<defs><linearGradient id=\"g\" gradientUnits=\"userSpaceOnUse\"></linearGradient></defs>"
      "<rect data-series=\"1\" width=\"10\" height=\"10\" fill=\"currentColor\"></rect></svg></figure>"))

(deftest svg-names-keep-case
  (let ((node (svg-canvas)))
    (ok (string= +svg-html+ (to-html node)) "el + to-html")
    (ok (string= +svg-html+ (to-html (render node))) "render")
    (ok (string= +svg-html+ (to-html (neutralize node))) "neutralize")
    (ok (contains (s+ "<body data-canvas-frozen=\"\">" +svg-html+ "</body>")
                  (freeze 'svg-canvas))
        "freeze")
    (ok (null (csp-violations node))))
  ;; Elements built directly (any name case) come out canonical too.
  (let ((raw (stack-html:make-html-element
              "svg" :attributes '(("VIEWBOX" . "0 0 1 1") ("CLASS" . "x")))))
    (ok (string= "<svg viewBox=\"0 0 1 1\" class=\"x\"></svg>" (to-html (neutralize raw)))))
  ;; Case-folded CSP checks still catch upper-case names on raw nodes.
  (let ((raw (stack-html:make-html-element
              "a" :attributes '(("ONCLICK" . "x") ("HREF" . "https://e.x/")))))
    (ok (equal '((:kind :event-handler :element "a" :attribute "onclick"))
               (csp-violations raw)))
    (ok (string= "<a></a>" (to-html (neutralize raw))))))
