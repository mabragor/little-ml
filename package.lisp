;;;; package.lisp

(defpackage #:little-ml
  (:use #:cl #:iterate)
  (:export #:make-cost-function #:1d-linear-regression #:regression-step))

