(defpackage :ocml.tests
  (:nicknames :tests) 
  (:use :common-lisp :ocml :5am)
  (:export run-all-tests))

(in-package :ocml.tests)

(defun run-all-tests (&optional (break nil))
  (let ((5am:*debug-on-failure* break))
    (run! 'namespaces-suite)
    (run! 'apples-suite)
    (run! 'owl-suite)))
