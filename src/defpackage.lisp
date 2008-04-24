(defpackage #:ocml
  (:use #:common-lisp)
  (:export #:*load-filename*
           #:as-ocml
           #:call-with-ocml-thread-safety
	   #:call-with-ontology
	   #:find-ontology-directory
           #:in-ocml
	   #:initialize-ocml
           #:load-ontology-by-name
	   #:register-namespace
           #:repackage
	   #:translate
	   #:with-ocml-thread-safety
	   #:with-ontology))

(eval-when (:load-toplevel)
 (pushnew :ocml cl:*features*)
 (pushnew :ocml-7.5 cl:*features*))
