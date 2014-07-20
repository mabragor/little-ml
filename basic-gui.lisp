;;; Heavily based on CLIM-FIG example from McCLIM package,
;;; which is distributed under LGPL
;;; Since this suite is distributed under ordinary GPL, I hope
;;; it'll not lead to any confusion.


(in-package #:cl-user)

(defpackage #:little-ml-demo
  (:use :clim :clim-lisp)
  (:export #:app-main #:app1-main #:app2-main #:little-ml-gui))

(in-package :little-ml-demo)

(defclass canvas-pane (application-pane)
  ((first-point-x :initform nil)
   (first-point-y :initform nil)))

(defun draw-figure (pane x y)
  (draw-ellipse* pane x y 20 0 0 20 :ink +black+ :line-style (make-line-style :thickness 10)))

(define-presentation-type figure ())

(define-presentation-method highlight-presentation
    ((type figure) record stream state)
  (declare (ignore record stream state))
  nil)

(defun handle-draw-object (pane x1 y1)
  (with-output-as-presentation (pane nil 'figure
				     :single-box t)
    (draw-figure pane x1 y1))
  (setf (little-ml-gui-redo-list *application-frame*) nil))

(defun handle-move-object (pane figure first-point-x first-point-y)
  (tracking-pointer (pane)
    (:pointer-button-release (&key event x y)
      (when (= (pointer-event-button event) +pointer-right-button+)
        (multiple-value-bind (old-x old-y)
            (output-record-position figure)
          (setf (output-record-position figure)
                (values (+ old-x (- x first-point-x))
                        (+ old-y (- y first-point-y)))))
        (window-refresh pane)
        (return-from handle-move-object)))))

(defun little-ml-gui ()
  (run-frame-top-level (make-application-frame 'little-ml-gui)))




(define-application-frame little-ml-gui ()
  ((output-record :accessor little-ml-gui-output-record)
   (redo-list :initform nil :accessor little-ml-gui-redo-list))
  (:panes
   (canvas (make-pane 'canvas-pane
		      :name 'canvas
                      :display-time nil)))
  (:layouts
   (default
       (vertically ()
	 (horizontally ()
	   (scrolling (:width 600 :height 400) canvas)))))
  (:top-level (default-frame-top-level :prompt 'clim-fig-prompt)))

(defmethod frame-standard-output ((frame little-ml-gui))
  (find-pane-named frame 'canvas))

(defmethod generate-panes :after (frame-manager (frame little-ml-gui))
  (declare (ignore frame-manager))
  (setf (little-ml-gui-output-record frame)
        ;; *standard-output* not bound to the canvas pane yet.
	(stream-current-output-record (frame-standard-output frame))))

(defun little-ml-gui-prompt (stream frame)
  (declare (ignore stream frame)))

(define-little-ml-gui-command com-exit ()
  (frame-exit *application-frame*))

(define-little-ml-gui-command (com-add-figure :name nil) ((x 'real) (y 'real))
  (handle-draw-object (find-pane-named *application-frame* 'canvas) x y))

(define-little-ml-gui-command (com-move-figure :name nil)
    ((figure 'figure) (x 'real) (y 'real))
  (handle-move-object (find-pane-named *application-frame* 'canvas)
                      figure x y))

(define-presentation-to-command-translator add-figure
    (blank-area com-add-figure little-ml-gui
                :gesture :select ; XXX
                :echo nil
                :tester ((window) (typep window 'canvas-pane)))
  (x y)
  (list x y))

(define-presentation-to-command-translator move-figure
    (figure com-move-figure little-ml-gui
            :gesture :menu ; XXX
            :echo nil)
  (presentation x y)
  (list presentation x y))

