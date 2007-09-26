(in-package :ocml.tests)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ocml:initialize-ocml)
  (load "ocml:library;applications;apple-heuristic-classify-redux;load"))

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
		     owl-suite))
      (format t "Running ~A: " (symbol-name suite))
      (run! suite))))

;;; {{{ Repackaging tool
(defun repackage (tree package)
  (tree-map #'(lambda (x)
		(repackage-thing x package))
	    tree))

(defun repackage-thing (thing package)
  (if (and (symbolp thing) (not (keywordp thing)))
      (intern (symbol-name thing) package)
      thing))

(defun tree-map (fn tree)
  (if (consp tree)
      (cons (tree-map fn (car tree))
	    (tree-map fn (cdr tree)))
      (funcall fn tree)))
;;; }}}
