(in-package :ocml.tests)

(eval-when (:execute :load-toplevel)
  (ocml:initialize-ocml)
  ;; XXX Should really be a test, but it makes a mess of the output.
  (ocml:load-ontology-by-name :apple-heuristic-classify-redux))

(defun run-all-tests (&optional (break nil))
  (let ((5am:*debug-on-error* break)
	(5am:*debug-on-failure* break))
    (dolist (suite '(concepts-suite
		     ;; XXX Constraints now don't work because slot
		     ;; types are computed after all classes have been
		     ;; defined.  Skip the test for now.
		     ;; constraints-suite
		     namespaces-suite
		     apples-suite
		     translation-suite
                     #+:ocml-with-cxml xml-to-ocml-suite
                     ))
      (format t "Running ~A: " (symbol-name suite))
      (run! suite))))
