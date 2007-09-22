(defpackage :ocml.tests
  (:nicknames :tests) 
  (:use :common-lisp :ocml :5am)
  (:export run-all-tests))

(in-package :ocml.tests)

(defun run-all-tests (&optional (break nil))
  (let ((5am:*debug-on-error* break)
	(5am:*debug-on-failure* break))
    (dolist (suite '(concepts-suite namespaces-suite apples-suite owl-suite))
      (format t "Running ~A: ~%" (symbol-name suite))
      (run! suite))))
