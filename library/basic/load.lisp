;;; Copyright (C) 2008 The Open University

(in-package #:ocml)

(def-ontology base-ontology
  "This is the basic building block of the OCML library. It contains the
   definitions required to do basic number, list and string manipulation.
   It also includes ontologies of sets, relations, functions, frames, tasks
   and problem solving methods"
 ;; :includes nil
  :do-not-include-base-ontology? t
  :type :basic
  :pathname "ocml:library;basic;"
  :author "enrico"
  :files ("foundation" "meta" "sets" "relations" "strings" "numbers"
                       "lists" "infer" "env" "functions"
		       "frames" "tasks" "mapping" "acquire" "time"))
