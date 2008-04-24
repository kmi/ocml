;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")


(in-ontology base-ontology)



(def-class FUNCTION ()  ?f
  "The class of OCML functions  This is an intensional definition, in the 
   sense that (function ?f) is true if ?f names a function"
  ((range :type unary-relation)
   ;;;;(domain :type unary-relation)
   (has-arity :type number :default-value 1)
   (has-body :type lambda-expression))
  :iff-def (or (defined-function ?f)
               (lambda-expression ?f)
               (member ?f (all-instances 'function))))


(def-class DEFINED-FUNCTION (function)
  "True for 'proper' functions and false for lambda expressions"
  :lisp-fun #'(lambda (x env)
                (if (ocml-function? 
                     (instantiate x env)) ;;;make sure to instantiate x
                  (list env)
                  :fail)))


(def-class LAMBDA-EXPRESSION (function) ?exp
   "The class of OCML lambda expressions"
   :iff-def (and (list ?exp)
                 (= ?exp (lambda ?schema ?body))
                 (schema ?schema)
                 (term ?body)))

(def-class PROCEDURAL-LAMBDA-EXPRESSION (lambda-expression) ?exp
   "The class of OCML lambda expressions used as procedures"
   :iff-def (and (list ?exp)
                 (= ?exp (lambda ?schema ?body))
                 (schema ?schema)
                 (procedural-expression ?body)))


(def-class PROCEDURE () ?f
   "The class of OCML procedures"
    :iff-def (or (defined-procedure ?f)
                 (procedural-lambda-expression ?f)
                 (member ?f (all-instances 'procedure))))

(def-class DEFINED-PROCEDURE (procedure)
  "True for 'proper' procedures and false for lambda expressions"
  :lisp-fun #'(lambda (x env)
                (if (procedure?
                     (instantiate x env)) ;;;make sure to instantiate x
                  (list env)
                  :fail)))


  

(def-class UNARY-PROCEDURE (procedure) ?f
  "A unary procedure has a single argument"
  ((has-arity :value 1))
 :iff-def (and (procedure ?f)
               (= (arity ?f)1)))

(def-function FUNCTION-SCHEMA (?f)
  :constraint (function ?rel)
  :body (if (lambda-expression ?f)
          (second ?f)
          (if (defined-function ?f)
            (defined-function-schema ?rel))))


(def-function DEFINED-FUNCTION-SCHEMA (?f)
  :constraint (defined-function ?f)
  :lisp-fun #'(lambda (x)
                 (schema (get-function x))))


(def-class UNARY-FUNCTION (function) ?f
  "A unary function is a function with a single argument"
  ((domain :type unary-relation)
    (has-arity :value 1))
 :iff-def (and (function ?f)
               (= (arity ?f)1)))


(def-class BINARY-FUNCTION (function) ?f
  "A binary function is a function with two arguments"
  ((has-arity :value 2))
  :iff-def (and (function ?f)
               (= (arity ?f)2)))



(def-relation DOMAIN (?f ?relation)
  "The relation domain is defined on unary functions and unary and binary relations.
   The domain of a function (or a relation) is a unary relation which is true for
   any possible input of the function (or first argument of the relation).  
   We define DOMAIN in a non-operational way"
  :constraint (and (unary-relation ?relation)
                   (or (unary-function ?f)
                       (unary-relation ?f)
                       (binary-relation ?f)))
  :iff-def (or
            (and (unary-function ?f)
                 (forall (?arg ?result)
                         (=> (= (apply ?f ?arg) ?result)
                             (holds ?relation ?arg))))
            (and (unary-relation ?f)
                 (forall (?arg1 )
                         (=> (holds ?f ?arg)
                             (holds ?relation ?arg))))
            (and (binary-relation ?f)
                 (forall (?arg1 ?arg2)
                         (=> (holds ?f ?arg1 ?arg2)
                             (holds ?relation ?arg1)))))
  :no-proofs-by (:iff-def))


(def-relation NTH-DOMAIN (?x ?n ?type)
  "From ontolingua......
   Domain restrictions generalized to n-ary relations.
   The sentence (nth-domain rel 3 type-class) says that
   the 3rd element of each tuple in the relation REL is
   an instance of type-class."
 :iff-def (and (positive-integer ?n)
                        (unary-relation ?type)
                        (or (function ?x)
                            (relation ?x))
                        (forall ?args
                                (=> (or (and (relation ?X)
                                             (goal-holds (cons ?x ?args)))
                                        (and (function ?X)
                                             (= (last ?args)(apply ?x (butlast ?args)))))
                                    (and 
                                     (>= (length ?args) ?n)
                                     (holds ?type (elt ?args (- ?n 1)))))))
  :no-proofs-by (:iff-def))

               
(def-relation RANGE (?f ?relation)
  "The range of a function or a binary relation is a relation which is true for
   any possible output of the function or second argument of the binary relation"
 :iff-def (or
                    (and (function ?f)
                         (forall (?args ?result)
                                 (=> (= (apply ?f ?args) ?result)
                                     (holds ?relation ?result))))
                    (and (binary-relation ?f)
                         (forall (?x ?y)
                                 (=> (holds ?f ?x ?y))
                                     (holds ?relation ?y))))
 :no-proofs-by (:iff-def))

;;;APPLY
;(def-function apply (?fun ?args)
;  :constraint (function ?fun)
 ; :lisp-fun #'ocml-apply-internal)

;;;CALL
;;(def-function call (?fun &rest ?args)
;;  :constraint (function ?fun)
;;  :body (apply ?fun ?args))

;;;(def-function EVAL (?fexp)
;;;;   "Evaluates a function or a procedure"
 ;;;;  :Lisp-fun #'ocml-eval-gen)


(def-function EVAL-FINDALL (?template ?goal)
  "Like FINDALL, except that it evaluates its arguments"
  :Lisp-fun #'findall)


(def-function EVAL-SETOFALL (?template ?goal)
  "Like SETOFALL, except that it evaluates its arguments"
  :Lisp-fun #'setofall)

(def-function EVAL-THE (?template ?goal)
  "Like THE, except that it evaluates its arguments"
  :Lisp-fun #'findany)

