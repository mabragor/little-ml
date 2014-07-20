;;;; little-ml.asd

(asdf:defsystem #:little-ml
  :serial t
  :description "Toy implementation of machine learning routines, following Andrew Ng's coursera course."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate #:cl-gtk2-gtk)
  :components ((:file "package")
               (:file "little-ml")
	       (:file "1dlinear-regression")))

(asdf:defsystem #:little-ml-demos
  :serial t
  :description "Demos, illustating work of ML algorithms in LITTLE-ML"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:little-ml
	       ;; #:cells-gtk
	       ;; #+cells-gtk-opengl #:cl-opengl
	       ;; #+cells-gtk-opengl #:cl-glu
	       ;; #+cells-gtk-opengl #:cl-glut)
	       #:mcclim)
  :components (;; (:file "demos")
	       (:file "basic-gui")))

(defsystem :little-ml-tests
  :description "Tests for LITTLE-ML."
  :licence "GPL"
  :depends-on (#:little-ml #:fiveam #:iterate)
  :serial t
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :little-ml))))
  (load-system :little-ml)
  (funcall (intern "RUN-TESTS" :little-ml)))
