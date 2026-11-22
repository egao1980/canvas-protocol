(defpackage #:canvas-protocol/tests
  (:use #:cl #:rove))

(in-package #:canvas-protocol/tests)

(deftest package-exists
  (ok (find-package :canvas-protocol)))
