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

(defun round-if-diff-small (x)
  (let ((round-x (round x)))
    (if (< (abs (- x round-x)) 0.02)
	round-x
	x)))

(defun teach-regression (n-steps regression)
  (iter (for i from 1 to (1- n-steps))
	(regression-step regression)
	(finally (return (regression-step regression)))))
  

(test 1d-linear-regression
  (is (equal '(1 1)
	     (mapcar #'round-if-diff-small
		     (teach-regression 1000
				       (make-instance '1d-linear-regression
						      :training-set '((1 2) (2 3) (3 4)))))))
  (is (equal '(1 0)
	     (mapcar #'round-if-diff-small
		     (teach-regression 1000
				       (make-instance '1d-linear-regression
						      :training-set '((1 1) (2 1) (3 1)))))))
  (is (equal '(0 1)
	     (mapcar #'round-if-diff-small
		     (teach-regression 1000
				       (make-instance '1d-linear-regression
						      :training-set '((1 1) (2 2) (3 3))))))))

      
