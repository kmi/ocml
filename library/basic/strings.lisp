;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

(def-class STRING (term)
   "A primitive class representing strings"
   :lisp-fun  #'(lambda (x env)
                 (if (stringp (instantiate x env)) ;;;make sure to instantiate x
                     (list env)
                     :fail)))

(def-function READ-FROM-STRING (?p)
  :constraint (string ?p)
  :lisp-fun #'read-from-string)

(def-function STRING-APPEND (?p1 ?p2)
  :constraint (and (string ?p1)(string ?p2))
  :lisp-fun #'string-append)

(def-function MAKE-STRING (?msg &rest ?args)
   :constraint (string ?msg)
   :lisp-fun #'(lambda (msg &rest args)
                 (apply #'format nil  msg args)))
