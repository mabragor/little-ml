;;;; This is part of little-ml-demos system, destributed under GPL

(defpackage :little-ml-demos
  (:use :common-lisp :pod :cells :gtk-ffi :cells-gtk :utils-kt :little-ml)
  (:export 1d-linear-regression-demo))

(in-package #:little-ml-demos)


;;;
;;; auxiliary color funcs
;;;

(defun highlight-col (rgb)
  (mapcar #'(lambda (val) (min 1 (+ val .3))) rgb))

(defun select-col (rgb)
  (mapcar #'(lambda (val) (max 0 (- val .3))) rgb))

(defmacro rgb? (rgb)
  (with-gensyms (col)
    `(c?
       (let ((,col ,rgb))
	(cond
	  ((mouse-over-p self) (highlight-col ,col))
	  ((selected-p self) (select-col ,col))
	  (t ,col))))))

(defmacro alpha? (alpha)
  (with-gensyms (a)
    `(c? (let ((,a ,alpha))
	   (cond
	     ((dragged-p self) .3)
	     (t ,a))))))

;;;
;;; random generators
;;;

(defun rnd (min max)
  (+ min (random max)))

(defun random-point (min-x min-y max-x max-y)
  (2d:v (rnd min-x max-x) (rnd min-y max-y)))

(defun random-color ()
  (loop for i from 0 below 3 collect (random 1.0)))

;;;
;;; drag'n'drop test
;;;

(defmodel test-cairo-dragging (hbox)
  ()
  (:default-initargs
      :fill t :expand t
      :kids (kids-list?
	     (make-instance 'cairo-drawing-area :md-name :draw :expand t :fill t
			    :fm-parent *parent*
			    :width 500 :height 500)
	     (mk-vbox
	      :kids (kids-list?
		     (list
		      (mk-button :label "Draw Box"
				 :on-clicked (callback (w e d)
					       (let* ((p1 (random-point 10 10 480 480))
						      (p2 (2d:v+ p1 (random-point 10 10 40 40)))
						      (col1 (random-color))
						      (col2 (random-color)))
						 (trcx "rect" p1 p2 col1 col2)
						 (mk-primitive (find-widget :draw) :rectangle
							       :p1 (c-in p1)
							       :p2 (c-in p2)
							       :rgb (rgb? col1)
							       :fill-rgb (rgb? col2)
							       :alpha (alpha? 1)
							       :filled t
							       :draggable t
							       :selectable t))))
		      (mk-button :label "Draw Arc"
				 :on-clicked (callback (w e d)
					       (let* ((p (random-point 10 10 480 480))
						      (radius (rnd 10 40))
						      (col1 (random-color))
						      (col2 (random-color)))
						 (mk-primitive (find-widget :draw) :arc
							       :p (c-in p)
							       :radius (c-in radius)
							       :rgb (rgb? col1)
							       :fill-rgb (rgb? col2)
							       :alpha (alpha? 1)
							       :filled t
							       :draggable t
							       :selectable t))))))))))


(defmodel 1d-linear-regression-gtk (gtk-app)
  ()
  (:default-initargs
      :title "1d linear regression demo"
    :position :center
    :width 650 :height 550
    :kids (c? (the-kids (list (make-instance 'test-cairo-dragging :fm-parent *parent*))))))


(defun 1d-linear-regression-demo ()
  (cells-gtk-init)
  (cells-gtk:start-app '1d-linear-regression-gtk))

;; (defparameter *inner-width* 40)
;; (defparameter *inner-height* 30)
;; (defparameter *out-in-ratio* 1)

;; (defparameter white-color (make-color :red 65535 :green 65535 :blue 65535))
;; (defparameter black-color (make-color :red 0 :green 0 :blue 0))
;; (defparameter *bg-color* white-color)

;; (defun setup-ratios (window)
;;   (multiple-value-bind (w h) (drawable-get-size (widget-window window))
;;     (setf *out-in-ratio* (/ w *inner-width*))
;;     (setf *inner-height* (/ h *out-in-ratio*))))

;; (defun prepare-window (window)
;;   (let* ((gc (graphics-context-new window)))
;;     (multiple-value-bind (w h) (drawable-get-size window)
;;       (setf (graphics-context-rgb-bg-color gc) *bg-color*)
;;       (setf (graphics-context-rgb-fg-color gc) *bg-color*)
;;       (draw-polygon window gc t (list (make-point :x 0 :y 0)
;; 				      (make-point :x w :y 0)
;; 				      (make-point :x w :y h)
;; 				      (make-point :x 0 :y h))))))

;; (defun 1d-linear-regression-demo ()
;;   (within-main-loop
;;     (let ((window (make-instance 'gtk-window :type :toplevel :app-paintable t))
;; 	  (eventbox (make-instance 'event-box)))
;;       (container-add window eventbox)
;;       (connect-signal window "destroy" (lambda (widget)
;; 					 (declare (ignore widget))
;; 					 (leave-gtk-main)))
;;       (connect-signal window "expose-event"
;; 		      (lambda (widget event)
;; 			(declare (ignore widget event))
;; 			(setup-ratios window)
;; 			(prepare-window (widget-window window))))
;;       (connect-signal window "configure-event"
;; 		      (lambda (widget event)
;; 			(declare (ignore widget event))
;; 			(setup-ratios window)
;; 			(prepare-window (widget-window window))
;; 			(widget-queue-draw window)))
;;       ;; (connect-signal eventbox "event"
;;       ;; 		      (lambda (widget event)
;;       ;; 			(declare (ignore widget))
;;       ;; 			(format *standard-output* "imhere~%")
;;       ;; 			(when (and (eq (event-type event) :button-release)
;;       ;; 				   (eq (event-button-button event) 1))
;;       ;; 			  (format *standard-output* "im there~%")
;;       ;; 			  (if (equal *bg-color* white-color)
;;       ;; 			      (setf *bg-color* black-color)
;;       ;; 			      (setf *bg-color* white-color))
;;       ;; 			  (prepare-window (widget-window window))
;;       ;; 			  (widget-queue-draw window))))
;;       (pushnew :button-press-mask (widget-events eventbox))
;;       (pushnew :key-press-mask (widget-events eventbox))
;;       (connect-signal eventbox "button-press-event"
;; 		      (lambda (widget event)
;; 			(declare (ignore widget event))
;; 			(format *standard-output* "imhere~%")
;; 			(ignore-errors
;; 			  (progn (format *standard-output* "im there~%")
;; 				 (if (equal *bg-color* white-color)
;; 				     (setf *bg-color* black-color)
;; 				     (setf *bg-color* white-color))
;; 				 (prepare-window (widget-window window))
;; 				 (widget-queue-draw window)))))
;;       (connect-signal eventbox "key-press-event"
;; 		      (lambda (widget event)
;; 			(declare (ignore widget))
;; 			(format *standard-output* "imhere~%")
;; 			(ignore-errors
;; 			  (let ((c (aref (gdk:event-key-string event) 0)))
;; 			    (case c
;; 			      (#\w (progn (format *standard-output* "im there~%")
;; 					  (if (equal *bg-color* white-color)
;; 					      (setf *bg-color* black-color)
;; 					      (setf *bg-color* white-color))
;; 					  (prepare-window (widget-window window))
;; 					  (widget-queue-draw window))))))))
;;       ;; (connect-signal window "key-press-event"
;;       ;; 		      #'gtk-window-propagate-key-event)
;;       ;; (gtk-main-add-timeout 10 (lambda ()
;;       ;; 				 (with-muffled-bare-ref-warn
;;       ;; 				   (collide *objects*)
;;       ;; 				   (calculate-forces *objects*)
;;       ;; 				   (move *objects*)
;;       ;; 				   (erase (widget-window window) *objects*)
;;       ;; 				   (draw (widget-window window) *objects*)
;;       ;; 				   t))
;;       ;; 			    :priority glib:+g-priority-high-idle+)
;;       (widget-realize eventbox)
;;       (widget-show window :all t))))
;; ;; (push :pointer-motion-mask (gdk-window-events (widget-window window))))))
