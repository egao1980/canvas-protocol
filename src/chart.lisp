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
  (error 'chart-backend-missing :chart chart))

(defun chart-node (chart &key (backend *chart-backend*))
  (unless (chart-p chart)
    (%canvas-error "chart-node needs a chart: ~S" chart))
  (restart-case (render-chart backend chart)
    (use-value (other-backend)
      :report "Render the chart with another backend."
      (chart-node chart :backend other-backend))))

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

(defun %invalid (field control &rest args)
  (error 'invalid-chart :field field :reason (apply #'format nil control args)))

(defun %point-ok-p (kind point)
  (ecase kind
    ((:line :scatter) (and (consp point) (realp (car point)) (realp (cdr point))))
    (:bar (and (consp point) (stringp (car point)) (realp (cdr point))))
    (:histogram (realp point))))

(defun %axis-values (kind series axis)
  "The real values the chart puts on AXIS (:x or :y)."
  (loop for s in series
        nconc (map 'list
                   (lambda (p)
                     (ecase kind
                       ((:line :scatter) (if (eq axis :x) (car p) (cdr p)))
                       (:bar (if (eq axis :x) nil (cdr p)))
                       (:histogram (if (eq axis :x) p nil))))
                   (series-points s))))

(defun %validate-series (kind series)
  (unless (and (consp series) (%proper-list-p series))
    (%invalid :series "a chart needs a non-empty list of series"))
  (dolist (s series)
    (unless (typep s 'series)
      (%invalid :series "not a series: ~S" s))
    (unless (stringp (series-name s))
      (%invalid :series "series name must be a string: ~S" (series-name s)))
    (let ((points (series-points s))
          (point-labels (series-labels s)))
      (unless (and (typep points 'sequence)
                   (or (vectorp points) (%proper-list-p points)))
        (%invalid :series "series ~S: points must be a list or vector" (series-name s)))
      (unless (every (lambda (p) (%point-ok-p kind p)) points)
        (%invalid :series "series ~S: points must be ~A for a ~(~A~) chart" (series-name s)
                  (ecase kind
                    ((:line :scatter) "(real . real)")
                    (:bar "(string . real)")
                    (:histogram "reals"))
                  kind))
      (when point-labels
        (unless (and (typep point-labels 'sequence)
                     (or (vectorp point-labels) (%proper-list-p point-labels))
                     (= (length point-labels) (length points)))
          (%invalid :series "series ~S: labels must match the points in length"
                    (series-name s))))))
  (let ((names (mapcar #'series-name series)))
    (unless (= (length names) (length (remove-duplicates names :test #'string=)))
      (%invalid :series "duplicate series names in ~S" names))))

(defun %validate-chart-axis (field axis points)
  (unless (typep axis 'axis)
    (%invalid field "not an axis: ~S" axis))
  (let ((min (axis-min axis))
        (max (axis-max axis))
        (reals (remove-if-not #'realp points)))
    (when (and min max (>= min max))
      (%invalid field "min ~A must be below max ~A" min max))
    (case (axis-scale axis)
      (:log
       (when (or (and min (<= min 0)) (and max (<= max 0)) (some (lambda (v) (<= v 0)) reals))
         (%invalid field "a log scale needs positive values and bounds")))
      (:time
       (unless (every (lambda (v) (typep v '(integer 0))) reals)
         (%invalid field "a time scale needs non-negative integer values (universal times)"))))))

(defun validate-chart (chart)
  "Return CHART or signal INVALID-CHART."
  (let ((kind (chart-kind chart))
        (series (chart-series chart)))
    (unless (member kind +chart-kinds+)
      (%invalid :kind "kind ~S is not one of ~S" kind +chart-kinds+))
    (%validate-series kind series)
    (%validate-chart-axis :x-axis (chart-x-axis chart) (%axis-values kind series :x))
    (%validate-chart-axis :y-axis (chart-y-axis chart) (%axis-values kind series :y))
    (when (eq kind :histogram)
      (unless (typep (chart-bins chart) '(integer 1))
        (%invalid :bins "bins must be a positive integer: ~S" (chart-bins chart))))
    (unless (typep (chart-width chart) '(integer 1))
      (%invalid :width "width must be a positive integer: ~S" (chart-width chart)))
    (unless (typep (chart-height chart) '(integer 1))
      (%invalid :height "height must be a positive integer: ~S" (chart-height chart)))
    (unless (member (chart-legend chart) '(:auto t nil))
      (%invalid :legend "legend must be :auto, T or NIL: ~S" (chart-legend chart)))
    chart))

(defun %validate-axis (axis)
  (unless (member (axis-scale axis) +axis-scales+)
    (%invalid :scale "scale ~S is not one of ~S" (axis-scale axis) +axis-scales+))
  (unless (or (null (axis-min axis)) (realp (axis-min axis)))
    (%invalid :min "min must be a real: ~S" (axis-min axis)))
  (unless (or (null (axis-max axis)) (realp (axis-max axis)))
    (%invalid :max "max must be a real: ~S" (axis-max axis)))
  (let ((ticks (axis-ticks axis)))
    (unless (and (%proper-list-p ticks) (every #'realp ticks))
      (%invalid :ticks "ticks must be a list of reals: ~S" ticks)))
  axis)

(defmethod render ((c chart))
  (restart-case (chart-node c)
    (use-placeholder ()
      :report "Render a placeholder figure instead of the chart."
      (el :figure '(:data-chart "missing") (el :figcaption nil (chart-title c))))))

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
