
(in-package #:cl-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export #:app-main #:app1-main #:app2-main))

(in-package #:app)

(define-application-frame superapp ()
  ((current-number :initform nil :accessor current-number))
  (:pointer-documentation t)
  (:panes
   (app :application :display-function 'display-app :height 400 :width 600)
   (int :interactor :height 200 :width 600))
  (:layouts
   (default (vertically () app int))))

(defun display-app (frame pane)
  (let ((number (current-number frame)))
    (format pane "~a is ~a"
	    number
	    (cond ((null number) "not a number")
		  ((oddp number) "odd")
		  (t "even")))))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-superapp-command (com-parity :name t) ((number 'integer))
  (setf (current-number *application-frame*) number))


  ;; (format t "~a is ~a~%" number
  ;; 	  (if (oddp number) "odd" "even")))

(define-application-frame superapp1 ()
  ((numbers :initform (loop repeat 20 collect (list (random 100000000)))
	    :accessor numbers)
   (cursor :initform 0 :accessor cursor))
  (:pointer-documentation t)
  (:panes
   (app :application :height 400 :width 600 :incremental-redisplay t :display-function 'display-app1)
   (int :interactor :height 200 :width 600))
  (:layouts (default (vertically () app int))))

(defun display-app1 (frame pane)
  (loop for element in (numbers frame)
       for line from 0
       do (princ (if (= (cursor frame) line) "*" " ") pane)
       do (updating-output (pane :unique-id element
				 :id-test #'eq
				 :cache-value (car element)
				 :cache-test #'eql)
	    (format pane "~a~%" (car element)))))

(defun app1-main ()
  (run-frame-top-level (make-application-frame 'superapp1)))

(define-superapp1-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-superapp1-command (com-add :name t) ((number 'integer))
  (incf (car (elt (numbers *application-frame*)
		  (cursor *application-frame*)))
	number))

(define-superapp1-command (com-next :name t) ()
  (incf (cursor *application-frame*))
  (when (= (cursor *application-frame*)
	   (length (numbers *application-frame*)))
    (setf (cursor *application-frame*) 0)))

(define-superapp1-command (com-prev :name t) ()
  (decf (cursor *application-frame*))
  (when (minusp (cursor *application-frame*))
    (setf (cursor *application-frame*)
	  (1- (length (numbers *application-frame*))))))

(define-application-frame superapp2 ()
  ((current-number :initform nil :accessor current-number))
  (:pointer-documentation t)
  (:panes
   (app :application :display-time t :height 300 :width 600)
   (int :interactor :height 200 :width 600))
  (:layouts
   (default (vertically () app int))))

(defun app2-main ()
  (run-frame-top-level (make-application-frame 'superapp2)))

(define-superapp2-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-presentation-type name-of-month ()
  :inherit-from 'string)

(define-presentation-type day-of-month ()
  :inherit-from 'integer)

(define-superapp2-command (com-out :name t) ()
  (with-output-as-presentation (t "The third month" 'name-of-month)
    (format t "March~%"))
  (with-output-as-presentation (t 15 'day-of-month)
    (format t "fifteen~%")))

(define-superapp2-command (com-get-date :name t)
    ((name 'name-of-month) (date 'day-of-month))
  (format (frame-standard-input *application-frame*)
	  "the ~a of ~a~%" date name))


(defclass person ()
  ((%last-name :initarg :last-name :accessor last-name)
   (%first-name :initarg :first-name :accessor first-name)
   (%address :initarg :address :accessor address)
   (%membership-number :initarg :membership-number :reader membership-number)))

(defun make-person (last-name first-name address membership-number)
  (make-instance 'person
		 :last-name last-name
		 :first-name first-name
		 :address address
		 :membership-number membership-number))

(defparameter *members*
  (list (make-person "Doe" "Jane" "123, Glence Terrace" 12345)
	(make-person "Dupont" "Jean" "111, Rue de la Republique" 54321)
	(make-person "Smith" "Eliza" "22, Trafalgar Square" 121212)
	(make-person "Nilsson" "Sven" "Uppsalagatan 33" 98765)))

(defclass members-view (view) ())

(defparameter *members-view* (make-instance 'members-view))

(define-application-frame views ()
  ((%members :initform *members* :accessor members))
  (:panes
   (main-pane :application :height 500 :width 500
	      :display-function 'display-main-pane
	      :default-view *members-view*)
   (interactor :interactor :height 100 :width 500))
  (:layouts
   (default (vertically ()
	      main-pane
	      interactor))))

(defgeneric display-pane-with-view (frame pane view))

(defun display-main-pane (frame pane)
  (display-pane-with-view frame pane (stream-default-view pane)))

(defmethod display-pane-with-view (frame pane (view members-view))
  (loop for member in (members frame)
       do (with-output-as-presentation
	      (pane member 'person)
	    (format pane "~a, ~a, ~a, ~a~%"
		    (membership-number member)
		    (last-name member)
		    (first-name member)
		    (address member)))))

(defclass person-view (view)
  ((%person :initarg :person :reader person)))

(defmethod display-pane-with-view (frame pane (view person-view))
  (let ((person (person view)))
    (format pane "Last name: ~a~%First name: ~a~%Address: ~a~%Membership number: ~a~%"
	    (last-name person)
	    (first-name person)
	    (address person)
	    (membership-number person))))

(defun views-example ()
  (run-frame-top-level (make-application-frame 'views)))

(define-views-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-views-command (com-show-all :name t) ()
  (setf (stream-default-view *standard-output*) *members-view*))

(define-views-command (com-show-person :name t) ((person 'person))
  (setf (stream-default-view *standard-output*)
	(make-instance 'person-view :person person)))
