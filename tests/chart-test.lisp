(in-package #:canvas-protocol/tests)

(defun line-series () (list (make-series "a" '((0 . 1) (1 . 2)))))

(defun chart-field (thunk)
  (handler-case (progn (funcall thunk) nil)
    (invalid-chart (c) (invalid-chart-field c))))

(deftest chart-valid-kinds
  (let ((c (make-chart :line :series (line-series) :title "T" :description "D")))
    (ok (chart-p c))
    (ok (eq :line (chart-kind c)))
    (ok (string= "T" (chart-title c)))
    (ok (string= "D" (chart-description c)))
    (ok (= 640 (chart-width c)))
    (ok (= 360 (chart-height c)))
    (ok (eq :auto (chart-legend c)))
    (ok (= 10 (chart-bins c)))
    (ok (eq :linear (axis-scale (chart-x-axis c))))
    (ok (eq :linear (axis-scale (chart-y-axis c))))
    (ok (string= "a" (series-name (first (chart-series c))))))
  (ok (chart-p (make-chart :scatter :series (list (make-series "s" #((1 . 2) (3/2 . 4.5d0)))))))
  (ok (chart-p (make-chart :bar :series (list (make-series "b" '(("x" . 1) ("y" . 2)))))))
  (ok (chart-p (make-chart :histogram :series (list (make-series "h" '(1 2 2 3))) :bins 3)))
  (ok (chart-p (make-chart :line :series (line-series)
                                 :y-axis (make-axis :scale :log :label "ms" :min 1 :max 100)
                                 :x-axis (make-axis :scale :time))))
  (ok (chart-p (make-chart :line :series (list (make-series "a" '((0 . 1)) :labels '("first") :color "#123456"))
                                 :legend nil :width 300 :height 200))))

(deftest chart-invalid-fields
  (loop for (expected thunk)
          in (list
              (list :kind (lambda () (make-chart :pie :series (line-series))))
              (list :series (lambda () (make-chart :line :series nil)))
              (list :series (lambda () (make-chart :line :series (list "not a series"))))
              (list :series (lambda () (make-chart :line :series (list (make-series "a" '((0 . "x")))))))
              (list :series (lambda () (make-chart :line :series (list (make-series "a" '((0 1)))))))
              (list :series (lambda () (make-chart :bar :series (list (make-series "b" '((1 . 2)))))))
              (list :series (lambda () (make-chart :histogram :series (list (make-series "h" '("a"))))))
              (list :series (lambda () (make-chart :line :series (list (make-series "a" '((0 . 1)))
                                                                       (make-series "a" '((0 . 2)))))))
              (list :series (lambda () (make-chart :line :series (list (make-series "a" '((0 . 1))
                                                                                    :labels '("x" "y"))))))
              (list :bins (lambda () (make-chart :histogram :series (list (make-series "h" '(1))) :bins 0)))
              (list :y-axis (lambda () (make-chart :line :series (list (make-series "a" '((1 . 0))))
                                                         :y-axis (make-axis :scale :log))))
              (list :x-axis (lambda () (make-chart :line :series (line-series)
                                                         :x-axis (make-axis :min 5 :max 1))))
              (list :x-axis (lambda () (make-chart :line :series (list (make-series "a" '((1.5 . 1))))
                                                         :x-axis (make-axis :scale :time))))
              (list :y-axis (lambda () (make-chart :line :series (line-series)
                                                         :y-axis (make-axis :scale :log :min 0))))
              (list :width (lambda () (make-chart :line :series (line-series) :width 0)))
              (list :height (lambda () (make-chart :line :series (line-series) :height "a")))
              (list :legend (lambda () (make-chart :line :series (line-series) :legend :sideways)))
              (list :scale (lambda () (make-axis :scale :weird)))
              (list :min (lambda () (make-axis :min "0")))
              (list :ticks (lambda () (make-axis :ticks '(1 "two")))))
        for i from 0
        do (ok (eq expected (chart-field thunk)) (format nil "case ~D -> ~S" i expected))))

(deftest invalid-chart-condition
  (let ((c (handler-case (make-chart :pie :series (line-series)) (invalid-chart (c) c))))
    (ok (typep c 'canvas-error))
    (ok (plusp (length (invalid-chart-reason c))))
    (ok (contains "kind" (princ-to-string c)))))

(deftest chart-constants
  (ok (equal '(:line :bar :scatter :histogram) +chart-kinds+))
  (ok (equal '(:linear :log :time) +axis-scales+))
  (ok (= 8 (length *chart-palette*)))
  (ok (string= "#E69F00" (first *chart-palette*)))
  (ok (every (lambda (c) (and (= 7 (length c)) (char= #\# (char c 0)))) *chart-palette*)))

(deftest render-chart-without-backend
  (let ((chart (make-chart :line :series (line-series) :title "T")))
    (let ((c (handler-case (render-chart nil chart) (chart-backend-missing (c) c))))
      (ok (typep c 'chart-backend-missing))
      (ok (typep c 'canvas-error))
      (ok (eq chart (chart-backend-missing-chart c)))
      (ok (contains "backend" (princ-to-string c))))
    (let ((*chart-backend* nil))
      (ok (signals (chart-node chart) 'chart-backend-missing)))))

(deftest chart-node-with-backends
  (let ((chart (make-chart :line :series (line-series) :title "T"))
        (b (make-instance 'test-chart-backend)))
    (ok (string= "<svg role=\"img\" viewBox=\"0 0 10 10\"><title>T</title></svg>"
                 (to-html (chart-node chart :backend b))))
    (ok (eq chart (first (backend-seen b))))
    (let ((*chart-backend* b))
      (ok (stack-html:html-element-p (chart-node chart)))
      (ok (contains "<svg" (to-html (card chart)))))
    (let ((*chart-backend* nil))
      (ok (contains "<svg"
                    (to-html (handler-bind ((chart-backend-missing
                                              (lambda (c) (declare (ignore c))
                                                (invoke-restart 'use-value b))))
                               (chart-node chart))))))))

(deftest render-chart-placeholder
  (let ((*chart-backend* nil)
        (chart (make-chart :bar :series (list (make-series "b" '(("x" . 1)))) :title "Sales <q>")))
    (ok (string= "<figure data-chart=\"missing\"><figcaption>Sales &lt;q&gt;</figcaption></figure>"
                 (to-html (handler-bind ((chart-backend-missing
                                           (lambda (c) (declare (ignore c))
                                             (invoke-restart 'use-placeholder))))
                            (render chart)))))
    (ok (signals (to-html chart) 'chart-backend-missing))))
