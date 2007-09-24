(in-package :ocml.tests)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ocml:initialize-ocml)
  (load "ocml:library;applications;apple-heuristic-classify-redux;load"))

(defun run-all-tests (&optional (break nil))
  (let ((5am:*debug-on-error* break)
	(5am:*debug-on-failure* break))
    (dolist (suite '(concepts-suite namespaces-suite apples-suite owl-suite))
      (format t "Running ~A: " (symbol-name suite))
      (run! suite))))
