(defpackage #:ocml
  (:use #:common-lisp)
  (:export #:*load-filename*
           #:call-with-ocml-thread-safety
	   #:call-with-ontology
	   #:find-ontology-directory
	   #:initialize-ocml
           #:load-ontology-by-name
	   #:register-namespace
	   #:translate
	   #:with-ocml-thread-safety
	   #:with-ontology))

(eval-when (:load-toplevel)
 (pushnew :ocml cl:*features*)
 (pushnew :ocml-7.5 cl:*features*))
