(in-package :cl-user)

(defpackage :little-ml-tests
  (:use :cl :little-ml :fiveam #:iterate)
  (:export #:run-tests))

(in-package :little-ml-tests)

(def-suite little-ml)
(in-suite little-ml)

(defun run-tests ()
  (let ((results (run 'little-ml)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test basic
  (multiple-value-bind (1dcost 1dtheta0-derivative 1dtheta1-derivative)
      (make-cost-function #(1 2 3) #(2 3 4))
    (is (equal 0 (funcall 1dcost 1 1)))
    (is (equal 0 (funcall 1dtheta0-derivative 1 1)))
    (is (equal 0 (funcall 1dtheta1-derivative 1 1)))
    (is (equal 7/3 (funcall 1dcost 1 0)))
    (is (equal -2 (funcall 1dtheta0-derivative 1 0)))
    (is (equal -14/3 (funcall 1dtheta1-derivative 1 0)))))

