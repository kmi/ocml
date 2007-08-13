;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

;;;CLASS VARIABLE
(def-class variable (term) ?x
  "True of an OCML variable"
  :lisp-fun #'(lambda (x env)
                (if
                  (variable? (instantiate x env))
                  (list env)
                  :fail)))

(def-relation variable-bound (?var)
  "true is ?var is bound or if it is not a variable"
  :lisp-fun #'(lambda (x env)
                (if
                  (unbound-variable? x env)
                  :fail
                  (list env))))

;;;BOOLEAN
(def-class boolean () ?X
   "A primitive class representing the set {true false}"
      :iff-def (member ?x (true false)))


;;; FAIL ---Always fails
(def-relation fail ()
   :lisp-fun #'(lambda (env)
		 (declare (ignore env))
		 :fail))


;;;TRUE ---Always succeeds
(def-relation true ()
   :lisp-fun #'(lambda (env) (list env)))


(def-class RELATION () ?r ;;;(set) ?r
   "The class of OCML relations.  This is an intensional definition, in the 
   sense that (relation ?r) is true if ?r names a relation"
   ((has-arity :type number))
   :iff-def (or (= ?r 'holds)
                (defined-relation ?r)
               (kappa-expression ?r)
                (member ?r (all-instances 'relation))))



(def-class KAPPA-EXPRESSION (relation) ?rel
   "Relations defined by means of the kappa operator"
   :iff-def (and (== ?rel (kappa ?schema ?exp))
                    (schema ?schema)
                    (sentence ?exp)))

(def-class UNARY-KAPPA-EXPRESSION (kappa-expression) ?rel
   "Relations defined by means of the kappa operator"
   :iff-def (and (kappa-expression ?rel)
                 (= (length (the-schema ?rel)) 1)))

(def-class DEFINED-RELATION (relation) ?rel
  "The class of all 'proper' relations - i.e. those which are not
   kappa expressions"
   :lisp-fun #'(lambda (x env)
                 (let ((y (unbound-variable? x env)))
                   (if y
                     (mapcar #'(lambda (rel)
                                   (cons (cons y rel) env))
                             (all-relations))
                     (if (get-relation (instantiate x env)) ;;;make sure to instantiate x
                       (list env)
                       :fail)))))


;;;RELATION <>
(def-relation <> (?x ?y)
   "True if ?x and ?y do not unify"
   :iff-def (not (= ?x ?y)))


;;;RELATION =
(def-relation = (?x ?y)
   "True if ?x and ?y do unify"
   :lisp-fun #'(lambda ( x y env)
                 (Let ((result (unify x y env)))
                   (if (eq result :fail)
                       :fail
                       (List result)))))

(def-relation == (?x ?y)
   "True if ?x and ?y do unify and they also have the same structure.
    This means that either they are both atoms, or they are lists with 
    the same structure"
   :lisp-fun #'(lambda ( x y env)
                 (Let ((result (unify-strong x y env)))
                   (if (eq result :fail)
                       :fail
                       (List result)))))


(def-function THE-SCHEMA (?x)
  "The schema of a function or relation"
  :constraint (or (function ?X)(relation ?X))
  :body (if (== ?x (lambda ?vars ?exp))
          ?vars
          (if (relation ?x)
            (relation-schema ?x)
            (if (function ?x)
              (function-schema ?x)
              :nothing))))


(def-function RELATION-SCHEMA (?rel)
  :constraint (relation ?rel)
  :body (if (kappa-expression ?rel)
          (second ?rel)
          (if (defined-relation ?rel)
            (defined-relation-schema ?rel))))


(def-function DEFINED-RELATION-SCHEMA (?rel)
  :lisp-fun #'(lambda (x)
                 (rename-variables (schema (get-relation x)))))


(def-relation SCHEMA (?thing)
  :iff-def (and (list ?thing)
                   (every ?thing variable)))

(def-function ARITY (?x)
  "The arity of a function or relation"
  :constraint (or (function ?X)(relation ?X))
  :body  (if (has-arity ?x ?n)
           ?n
           (if (= ?x 'holds)
             nil
          
           (in-environment 
           ((?l . (the-schema ?x))
            (?n . (length ?l)))
           (if (every ?l variable)
             ?n
             ;we assume that the only non-var can be &rest
             (- ?n 1))))))
         
          
     
(def-function EXTENSION (?r) -> ?set
  "The extension of a relation is the set of all tuples for which the relation
   holds.  This is a kind of operational definition, which retrieves the set of all
   tuples for which the relation is predicated in the current KB.  This function
   is restricted to defined relations only"
  :constraint (defined-relation ?r)
  :body (if (= (the-schema ?r) ?list)
	    (eval-setofall ?list (cons ?r ?list))))


(def-relation FUNCTIONAL-RELATION (?r)
  "This is true if the extension of a relation r is functional - 
   i.e. there are no two tuples (xo, ....,xn), (y0,.....,yn) in the
   extension of the relation, such that (x0,....xn-1) = (y0,.....,yn-1)
   and xn <> yn"
  :iff-def (and (defined-relation ?r)
                (functional-extension-p (extension ?r))))



(def-relation INVERSE-FUNCTIONAL-RELATION (?r)
  "If a relation is inverse-functional, then its range provides a unique identifier"
  :iff-def (forall (?y ?X ?z)
                   (=> (and (defined-relation ?r)
                            (holds ?r ?y ?x)
                            (holds ?r ?z ?x))
                       (same-individual ?y ?z))))



(def-relation FUNCTIONAL-EXTENSION-P (?tuples)
  :iff-def (and (set-as-list ?tuples)
                (not (exists (?t1 ?t2)
                             (and (member ?t1 ?tuples)
                                  (member ?t2 ?tuples)
                                  (> (length ?t1) 1)
                                  (= (butlast ?t1)(butlast ?t2))
                                  (<> (last ?t1)(last ?t2)))))))
           
(def-function UNIVERSE (?r) -> ?set
  "The universe of a relation is the set of all objects occurring in some
   instance of the relation.  This is a kind of operational definition, which 
   considers only the instances which can be proven"
  :constraint (defined-relation ?r)
  :body (setofall ?X (exists ?l
                             (and (member ?l (extension ?r))
                                  (member ?x ?l)))))


(def-class UNARY-RELATION (relation) ?r
  ((domain :type unary-relation)
   (has-arity :value 1))
  :iff-def (and (relation ?r) 
                (= (arity ?r) 1)))


(def-class BINARY-RELATION (relation) ?r 
  ((has-arity :value 2)
   (domain :type unary-relation)
   (range :type unary-relation))
  :iff-def (and (relation ?r) 
                (= (arity ?r) 2)))

(def-class ORDER-RELATION (binary-relation) ?r
  :iff-def (defines-partial-order ?r))


(def-function INVERSE-EXTENSION (?r) -> ?tuples
  "The inverse extension of a binary relation is generating 
   by reversing each tuple in the extension of the 
   relation"
  :constraint (binary-relation ?r)
  :body (setofall (?y ?x) (holds ?r ?x ?y)))


(def-function COMBINED-EXTENSION (?r1 ?r2) -> ?relation
 "The combined extension of  binary relations r1 and r2
  is the set of pairs (x z), such that there is an object y 
  such that x is related to y  by r1 and y is related to z by r2."
 :constraint (and (binary-relation ?r1)(binary-relation ?r2))
  :body (setofall ( ?x ?z)
                  (exists (?y)
                          (and (holds (?r1 ?x ?y))
                               (holds (?r2 ?y ?z))))))

(def-relation ONE-ONE (?r)
  :iff-def (and  (binary-relation ?r)
                (functional-relation-p ?r)
                (functional-extension-p (inverse-extension ?r))))

(def-relation MANY-ONE (?r) 
  :iff-def (and (binary-relation ?r)
                (functional-relation-p ?r)))

(def-relation ONE-MANY (?r) 
  :iff-def (and (binary-relation ?r)
                (functional-extension-p (inverse-extension ?r))))

(def-relation MANY-MANY (?r) 
  :iff-def (and (binary-relation ?r)
                (not (functional-relation-p ?r))
                (not (functional-extension-p (inverse-extension ?r)))))

(def-relation TRANSITIVE (?R)
  "A relation is transitive if (r x y) and (r y z) => (r x z)
   To avoid this to be trivially true for empty relations we 
   first check that there are at least two relation tuples"
  :iff-def (and (relation ?r)
                (not (= ?r holds)) ;;;holds is a special case
                (holds ?r ?x ?y)
                (holds ?r ?y ?z)
                (<> ?y ?z)
		(not (exists (?x ?y ?z)
                             (and (holds ?r ?x ?y)
                                  (holds ?r ?y ?z)
                                  (<> ?y ?z)
                                  (not (holds ?r ?x ?z)))))))

(def-relation SYMMETRIC (?R)
  "A relation is symmetric iff (r x y)  => (r y x )"
 
  :iff-def (and (relation ?r)
                (not (exists (?x ?y)
                             (and (holds ?r ?x ?y)
                                  (not (holds ?r ?y ?x)))))))

(def-relation INVERSE-OF (?R1 ?R2)
  :iff-def (and (relation ?r1)(relation ?r2)
                (not (exists (?x ?y)
                             (and (holds ?r1 ?x ?y)
                                  (not (holds ?r2 ?y ?x)))))))

(def-relation REFLEXIVE (?R)
  "A relation is reflexive if (r x x) is true for all x"
  :iff-def (and (relation ?r)
                (not (exists (?x)
                             (and (holds ?r ?x ?y)
                                  (not (holds ?r ?x ?x)))))))

(def-relation IRREFLEXIVE (?R)
  "A relation is irreflexive if (r x x) is false for any x"
  :iff-def (and (relation ?r)
                (not (exists (?x)
                             (holds ?r ?x ?x)))))


(def-relation DEFINES-TOTAL-ORDER (?R)
  "A relation defines a total order if it is transitive,
  irreflexive, and for each x, y either (r x y) or (r y x) is true" 
  :iff-def (and (relation ?r)
                (transitive ?r)
                (irreflexive ?r)
                (not (exists (?x ?y )
                             (and (not (holds ?r ?x ?y))
				  (not (holds ?r ?y ?x)))))))

(def-relation DEFINES-PARTIAL-ORDER (?R)
  "A relation defines a partial order if it is transitive and
  irreflexive" 
  :iff-def (and (relation ?r)
                (transitive ?r)
                (irreflexive ?r)
                ))

(def-relation SUBRELATION-OF (?child-relation ?parent-relation)
  :iff-def (and (relation ?child-relation)
                (relation ?parent-relation)
                (not (exists ?x
                             (and (holds ?child-relation ?X)
                                  (not (holds ?parent-relation ?x)))))))



