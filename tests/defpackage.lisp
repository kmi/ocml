(defpackage :ocml.tests
  (:nicknames :tests) 
  (:use :common-lisp :ocml :5am)
  (:export run-all-tests))

(in-package :ocml.tests)

(defun run-all-tests ()
  (run! 'apples-suite)
  (run! 'owl-suite))
