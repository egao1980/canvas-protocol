(in-package #:canvas-protocol)

;;; Chart series colours for the themes, built with css-protocol.
;;;
;;; Chart backends paint marks in currentColor and tag each series with
;;; data-series="N", N = 1 + (series index mod (length *chart-palette*)),
;;; 1-based (canvas-backend-svg's PALETTE-CSS contract). A theme turns N into
;;; a colour:
;;;
;;; CHART-STYLESHEET &key (light *chart-palette*) (dark *chart-palette-dark*)
;;;   -> css-protocol STYLESHEET with, in order:
;;;   1. the custom-property theme (CSS:MAKE-CSS-THEME on :root, no toggle
;;;      attribute, like the placeholder theme): --chart-1..N from LIGHT, and
;;;      under @media (prefers-color-scheme: dark) from DARK;
;;;   2. for each N, the rule canvas-backend-svg's PALETTE-CSS prints:
;;;        figure[data-chart] [data-series="N"]:not([color]) { color: var(--chart-N, LIGHT-N); }
;;;      The :not([color]) keeps a caller's explicit series colour (an SVG
;;;      color presentation attribute).
;;;   LIGHT and DARK must have the same length, else CANVAS-ERROR.
;;; CHART-CSS &rest keys -> the same stylesheet as text, each top-level node
;;;   minified on its own line, ending in a newline. assets/canvas.css ends
;;;   with exactly (CHART-CSS) (tested); regenerate that block when the
;;;   palettes change.
;;;
;;; *CHART-PALETTE-DARK*: Okabe-Ito with black swapped for a light grey (black
;;; is unreadable on a dark background); the other seven keep at least 3:1
;;; against the placeholder's dark --bg #0d1117 (WCAG 1.4.11, graphics).

(defparameter *chart-palette-dark*
  '("#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#E6EDF3")
  "*CHART-PALETTE* for dark backgrounds, in series order.")

(defun %chart-properties (palette)
  (loop for colour in palette
        for n from 1
        collect (cons (format nil "chart-~D" n) colour)))

(defun chart-stylesheet (&key (light *chart-palette*) (dark *chart-palette-dark*))
  (unless (= (length light) (length dark))
    (%canvas-error "chart palettes differ in length: ~D light, ~D dark"
                   (length light) (length dark)))
  (stack-css:merge-stylesheets
   (stack-css:css-theme-stylesheet
    (stack-css:make-css-theme :light (%chart-properties light)
                              :dark (%chart-properties dark)))
   (stack-css:make-stylesheet
    (loop for colour in light
          for n from 1
          collect (stack-css:make-rule
                   (format nil "figure[data-chart] [data-series=\"~D\"]:not([color])" n)
                   (list (stack-css:make-declaration
                          "color" (format nil "var(--chart-~D, ~A)" n colour))))))))

(defun chart-css (&rest keys &key light dark)
  (declare (ignore light dark))
  (with-output-to-string (out)
    (dolist (node (stack-css:stylesheet-rules (apply #'chart-stylesheet keys)))
      (stack-css:write-stylesheet node :stream out :pretty nil)
      (terpri out))))
