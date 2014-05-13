;;;; package.lisp

(defpackage #:little-ml
  (:use #:cl #:iterate #:gtk #:gdk #:gobject)
  (:export #:make-cost-function #:1d-linear-regression #:regression-step
	   #:1d-linear-regression-demo))

