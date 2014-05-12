;;;; Part of little-acl

(in-package #:little-ml)

(defun normalize-training-input (xs ys)
  ;; for now just do nothing
  (if (not ys)
      (iter (for pt in-sequence xs)
	    (collect (elt pt 0) into x-res)
	    (collect (elt pt 1) into y-res)
	    (finally (setf xs (coerce x-res 'vector)
			   ys (coerce y-res 'vector))))
      (progn (setf xs (coerce xs 'vector)
		   ys (coerce ys 'vector))))
  (if (equal (length xs) (length ys))
      (values xs ys (length xs))
      (error "Training inputs and output should have the same length.")))

(defun make-cost-function (xs &optional ys)
  (multiple-value-bind (xs ys len) (normalize-training-input xs ys)
    (let ((avg-x (/ (iter (for x in-vector xs)
			  (summing x))
		    len))
	  (avg-y (/ (iter (for y in-vector ys)
			  (summing y))
		    len))
	  (avg-xy (/ (iter (for y in-vector ys)
			   (for x in-vector xs)
			   (summing (* x y)))
		     len))
	  (avg-x2 (/ (iter (for x in-vector xs)
			   (summing (expt x 2)))
		     len))
	  (avg-y2 (/ (iter (for y in-vector ys)
			   (summing (expt y 2)))
		     len)))
      ;; (format t "avg-x ~a avg-y ~a avg-xy ~a avg-x2 ~a avg-y2 ~a~%" avg-x avg-y avg-xy avg-x2 avg-y2)
      (values (lambda (theta0 theta1)
		(+ (/ (expt theta0 2) 2)
		   (* theta0 theta1 avg-x)
		   (- (* theta0 avg-y))
		   (- (* theta1 avg-xy))
		   (/ (* (expt theta1 2) avg-x2) 2)
		   (/ avg-y2 2)))
	      (lambda (theta0 theta1)
		(+ theta0 (* theta1 avg-x) (- avg-y)))
	      (lambda (theta0 theta1)
		(+ (* theta0 avg-x)
		   (* theta1 avg-x2)
		   (- avg-xy)))))))

(defun calc-appropriate-time-step (cost-function gradient point alpha)
  (labels ((rec (cur-alpha min-cost higher-cost-hit)
	     ;; (format t "calculating time step cur-alpha ~a min-cost ~a begin-cost ~a higher-cost-hit ~a~%"
	     ;; cur-alpha min-cost (apply cost-function point) higher-cost-hit)
	     (let ((next-cost (apply cost-function (mapcar (lambda (x y)
							     (- x (* cur-alpha y)))
							   point gradient))))
	       (cond ((< next-cost min-cost) (if higher-cost-hit
						 cur-alpha
						 (rec (* 2 cur-alpha) next-cost higher-cost-hit)))
		     ((= next-cost min-cost) (if higher-cost-hit
						 cur-alpha
						 (rec (/ cur-alpha 2) min-cost t)))
		     (t (rec (/ cur-alpha 2) min-cost t))))))
    (rec alpha (apply cost-function point) nil)))
	    
(defun make-gradient-descent-stepper (cost-function cost-function-partial-derivatives)
  (let ((alpha 0.1))
    (lambda (point)
      (let ((gradient (mapcar (lambda (x)
				(apply x point))
			      cost-function-partial-derivatives)))
	(setf alpha (calc-appropriate-time-step cost-function gradient point alpha))
	(mapcar (lambda (x y)
		  (- x (* alpha y)))
		point gradient)))))
			    
(defclass 1d-linear-regression ()
  ((point :initarg :point)
   (cost-function)
   (gradient)
   (stepper)
   (training-set :initarg :training-set)))

(defgeneric plug-training-set-into-regression (regression training-set)
  )

(defmethod plug-training-set-into-regression ((regression 1d-linear-regression) training-set)
  (destructuring-bind (cost-function . gradient)
      (multiple-value-list (make-cost-function (slot-value regression 'training-set)))
    (setf (slot-value regression 'cost-function) cost-function
	  (slot-value regression 'gradient) gradient
	  (slot-value regression 'stepper) (make-gradient-descent-stepper cost-function gradient))))
     
(defmethod initialize-instance :after ((regression 1d-linear-regression) &key)
  (if (slot-boundp regression 'point)
      (if (not (= 2 (length (slot-value regression 'point))))
	  (error "Point of 1d linear regression has precisely two parameters."))
      (setf (slot-value regression 'point) (list (random 10.0) (random 10.0))))
  (if (slot-boundp regression 'training-set)
      (plug-training-set-into-regression regression (slot-value regression 'training-set))))

(defgeneric regression-step (regression)
  )
(defmethod regression-step ((regression 1d-linear-regression))
  (with-slots (point stepper) regression
    (setf point (funcall stepper point))))
  
