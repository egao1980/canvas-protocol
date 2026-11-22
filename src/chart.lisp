(in-package #:canvas-protocol)

;;; Chart model (backend-neutral) and the RENDER-CHART GF.
;;;
;;; MAKE-CHART kind &key series x-axis y-axis title description (legend :auto)
;;;   (width 640) (height 360) (bins 10) -> a validated CHART (VALIDATE-CHART).
;;;   X-AXIS / Y-AXIS default to (make-axis).
;;; Validation -> INVALID-CHART with FIELD:
;;;   :kind    not in +CHART-KINDS+
;;;   :series  empty; a non-SERIES element; duplicate names (STRING=); points
;;;            not of the kind's shape -- :line/:scatter (real . real),
;;;            :bar (string . real), :histogram reals; LABELS given with a
;;;            length different from the points'
;;;   :x-axis / :y-axis  min >= max when both given; :log scale with a value
;;;            (or min) <= 0 on that axis; :time scale with non-integer or
;;;            negative x values (universal times)
;;;   :bins    histogram bins not a positive integer
;;;   :width / :height  not a positive integer
;;;   :legend  not one of :auto T NIL
;;; MAKE-AXIS validates locally: scale not in +AXIS-SCALES+ -> :scale; min/max
;;;   not reals -> :min / :max; ticks not a list of reals -> :ticks.
;;; MAKE-SERIES name points &key color labels: NAME a string, POINTS a list or vector.
;;;
;;; RENDER-CHART backend chart &key -> an html-protocol node. The method on
;;; NULL signals CHART-BACKEND-MISSING. CHART-NODE chart &key (backend
;;; *chart-backend*) calls it with restart USE-VALUE (a backend to use instead).

(defparameter +chart-kinds+ '(:line :bar :scatter :histogram))
(defparameter +axis-scales+ '(:linear :log :time))

(defparameter *chart-palette*
  '("#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#000000")
  "Okabe-Ito colour-blind-safe palette, in series order.")

(defvar *chart-backend* nil
  "Backend used by CHART-NODE and RENDER of a chart (e.g. canvas-backend-svg's).")

(defclass chart-backend () ()
  (:documentation "Base class of chart backends."))

(defgeneric render-chart (backend chart &key)
  (:documentation "Render CHART with BACKEND to an html-protocol node."))

(defmethod render-chart ((backend null) chart &key)
  (declare (ignore chart))
  (%h4h0-pending 'render-chart))

(defun chart-node (chart &key (backend *chart-backend*))
  (declare (ignore chart backend))
  (%h4h0-pending 'chart-node))

(defclass axis ()
  ((label :initarg :label :initform nil :reader axis-label)
   (scale :initarg :scale :initform :linear :reader axis-scale)
   (min :initarg :min :initform nil :reader axis-min)
   (max :initarg :max :initform nil :reader axis-max)
   (ticks :initarg :ticks :initform nil :reader axis-ticks)
   (format :initarg :format :initform nil :reader axis-format)))

(defclass series ()
  ((name :initarg :name :reader series-name)
   (points :initarg :points :reader series-points)
   (color :initarg :color :initform nil :reader series-color)
   (labels :initarg :labels :initform nil :reader series-labels)))

(defclass chart ()
  ((kind :initarg :kind :reader chart-kind)
   (series :initarg :series :reader chart-series)
   (x-axis :initarg :x-axis :reader chart-x-axis)
   (y-axis :initarg :y-axis :reader chart-y-axis)
   (title :initarg :title :initform nil :reader chart-title)
   (description :initarg :description :initform nil :reader chart-description)
   (legend :initarg :legend :initform :auto :reader chart-legend)
   (width :initarg :width :initform 640 :reader chart-width)
   (height :initarg :height :initform 360 :reader chart-height)
   (bins :initarg :bins :initform 10 :reader chart-bins)))

(defun chart-p (object)
  (typep object 'chart))

(defun validate-chart (chart)
  "Return CHART or signal INVALID-CHART."
  (declare (ignore chart))
  (%h4h0-pending 'validate-chart))

(defun %validate-axis (axis)
  (declare (ignore axis))
  (%h4h0-pending 'make-axis))

(defun make-axis (&key label (scale :linear) min max ticks format)
  (%validate-axis (make-instance 'axis :label label :scale scale :min min :max max
                                       :ticks ticks :format format)))

(defun make-series (name points &key color labels)
  (check-type name string)
  (check-type points sequence)
  (make-instance 'series :name name :points points :color color :labels labels))

(defun make-chart (kind &key series x-axis y-axis title description (legend :auto)
                          (width 640) (height 360) (bins 10))
  (validate-chart
   (make-instance 'chart :kind kind :series series
                         :x-axis (or x-axis (make-axis)) :y-axis (or y-axis (make-axis))
                         :title title :description description :legend legend
                         :width width :height height :bins bins)))
