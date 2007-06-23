(defpackage ocml
  (:use common-lisp enrico-utilities)
  (:export call-with-ocml-thread-safety
	   call-with-ontology
	   register-namespace
	   with-ocml-thread-safety
	   with-ontology))

(eval-when (:load-toplevel)
 (push :ocml cl:*features*)
 (push :ocml-7.5 cl:*features*))
