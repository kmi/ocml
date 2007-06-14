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
  :pathname "OCML:LIBRARY;BASIC;"
  :author "enrico"
  :files ( "FOUNDATION"
                       "META7" 
                       "SETS2" 
                       "RELS2"  
                       "STRINGS"
                       "NUMS"
                       "LIST2"
                         
                       "INFER"
                          "ENV"
                          
                          "FUNS2"
                          
                         
                          
                          
                          
                          
                          ;;;;;"OPS2"
                          
                          "FRAMES4"
                         
                          "TASK6"
                          "MAPPING"
                          "ACQUIRE"))


