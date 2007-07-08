;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-



(in-package "OCML")

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
  :files ("foundation" "meta7" "sets2" "rels2" "strings" "nums"
                       "list2" "infer" "env" "funs2"
		       ;; "ops2"
		       "frames4" "task6" "mapping" "acquire"))


