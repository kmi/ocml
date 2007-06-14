;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)


(def-relation SAME-INDIVIDUAL (?X ?Y)
  "states that ?x and ?y are the same"
  :sufficient (and (inverse-functional-relation ?r)
                   (holds ?r ?x ?z)
                   (holds ?r ?y ?z)
                   (<> ?x ?y)))
                   

(def-relation DIFFERENT-INDIVIDUAL-FROM (?X ?Y)
  "states that ?x and ?y are the same"
  :sufficient (and (same-individual ?x ?z)
                   (<> ?z ?y)
                   (different-individual-from ?z ?y)))


(def-rule integrity-of-individuals 
  (same-individual ?x ?y)
  (different-individual-from ?x ?y)
  then
  (exec (output "Warning ~s and ~s are declared both to be the same and to be different!"
                  ?x ?y)))

(def-rule transitive-same-individual-rule
  (same-individual ?x ?y)
  (same-individual ?y ?z)
  then
  (exec (tell (same-individual ?x ?z))))

(def-rule symmetric-same-individual-rule
  (same-individual ?x ?y)
  
  then
  (exec (tell (same-individual ?y ?x))))

(def-axiom transitive-same-individual
  (transitive same-individual))

(def-axiom symmetric-same-individual
  (symmetric same-individual))



