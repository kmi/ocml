;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

(def-class OCML-THING () ?x
  "This is the top-level class. Any class will be a subclass of this one"
  ((has-pretty-name :type string
                     :max-cardinality 1
                     :documentation "A human readable name"))
  :exclusive-prove-by (and (variable-bound ?x)
                           (member ?x (all-instances 'ocml-thing))))

;;;(def-relation KNOWN-INSTANCE (?x)
;;;;  :iff-def (member ?X (all-instances ocml-thing)))

(def-class OCML-EXPRESSION (ocml-thing) ?x 
  :iff-def (or (sentence ?x)
               (procedural-expression ?x)
               (term ?x)
               (list ?x)))


;;;CLASS LIST
(def-class list (OCML-expression )?x
   "A  class representing lists."
   ((element-type :type class))
   :iff-def (or (= ?x nil)
                (== ?x (?a . ?b))))

;;;HOME-ONTOLOGY-OF
;;;(def-function home-ontology-of (?thing ?type)
;;;  :lisp-fun #'(lambda (x y)
;;;                (name (home-ontology-of x y))))


(def-function home-ontology-of-structure (?thing )
  :lisp-fun #'(lambda (x )
             (name (home-ontology x ))))





 