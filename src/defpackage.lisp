(defpackage #:ocml
  (:use #:common-lisp)
  (:export #:call-with-ocml-thread-safety
	   #:call-with-ontology
	   #:initialize-ocml
	   #:register-namespace
	   #:translate
	   #:with-ocml-thread-safety
	   #:with-ontology))

(eval-when (:load-toplevel)
 (pushnew :ocml cl:*features*)
 (pushnew :ocml-7.5 cl:*features*))
