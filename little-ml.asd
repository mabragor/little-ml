;;;; little-ml.asd

(asdf:defsystem #:little-ml
  :serial t
  :description "Toy implementation of machine learning routines, following Andrew Ng's coursera course."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate)
  :components ((:file "package")
               (:file "little-ml")
	       (:file "1dlinear-regression")))

(defsystem :little-ml-tests
  :description "Tests for LITTLE-ML."
  :licence "GPL"
  :depends-on (#:little-ml #:fiveam #:iterate)
  :serial t
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :little-ml))))
  (load-system :little-ml)
  (funcall (intern "RUN-TESTS" :little-ml)))
