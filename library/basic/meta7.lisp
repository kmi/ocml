;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

;;;Now in foundation.lisp

;;(def-class OCML-EXPRESSION () ?x 
;;  :iff-def (or (sentence ?x)
;;               (procedural-expression ?x)
;;               (term ?x)
;;               (list ?x)))

(def-class PROCEDURAL-EXPRESSION (ocml-expression) ?x
  "The class of OCML procedural expressions"
  :iff-def (or (term ?x)
               (tell-statement ?x)
               (unassert-statement ?x)
               (loop-statement ?x)
                (do-statement ?x)
                (repeat-statement ?x)
                (if-procedural-statement ?x)
                (cond-procedural-statement ?x)
                (in-env-procedural-statement ?x)
                (defined-procedural-expression ?x)))


(def-class TERM (ocml-expression) ?x
   "The class of OCML functional terms"
   :iff-def (or (string ?x)
                (number ?X)
                (variable ?x)
                (constant ?x)
                (quote-expression ?x)
                (findall-expression ?x)
                (the-expression ?x)
                (setofall-expression ?x)
                (if-term ?x)
                (cond-term ?x)
                (in-env-term ?x)
                (eval-expression ?x)
                (call-expression ?x)
                (apply-expression ?x)
                (defined-function-expression ?x)))

  
(def-relation tell-statement (?exp)
  :iff-def (and (list ?exp)
                (not (variable (first ?exp)))
                (== ?exp '(tell ?b))
                (basic-sentence ?b)))

(def-relation unassert-statement (?exp)
  :iff-def (and (list ?exp)
                (not (variable (first ?exp)))
                (== ?exp '(unassert ?b))
                (basic-sentence ?b)))

(def-relation loop-statement (?exp)
  :iff-def (and (list ?exp)
                (not (variable (first ?exp)))
                (== ?exp '(loop for ?var in ?term do . ?p-exps))
                (variable ?var)
                (term ?term)
                (every ?p-exps procedural-expression)))

(def-relation do-statement (?exp)
  :iff-def (and (list ?exp)
                (not (variable (first ?exp)))
                (== ?exp (do . ?exps))
                (every ?exps procedural-expression)))

(def-relation REPEAT-STATEMENT (?exp)
  :iff-def (verify-repeat-statement ?exp))

(def-rule verify-repeat-statement 
  ((verify-repeat-statement ?exp)
   if
   (list ?exp)
   (not (variable (first ?exp)))
   (== ?exp (repeat ?end-test . ?exps))
   (sentence ?end-test)
   (every ?exps procedural-expression))
  ((verify-repeat-statement ?exp)
   if
   (list ?exp)
   (not (variable (first ?exp)))
   (== ?exp (repeat . ?exps))
   (every ?exps procedural-expression))
  ((verify-repeat-statement ?exp)
   if
   (list ?exp)
   (not (variable (first ?exp)))
   (== ?exp (repeat . ?l))
   (= ?test (last ?l))
   (sentence ?last)
   (every (butlast ?l) procedural-expression)))


(def-relation if-procedural-statement (?x)
  :iff-def (verify-if-procedural-statement ?x))

(def-rule verify-if-procedural-statement 
  ((verify-if-procedural-statement ?x)
   if
   (list ?X)
   (not (variable (first ?x)))
   (== ?x '(if ?sentence ?exp1 ?exp2))
   (sentence ?sentence)
   (procedural-expression ?exp1)
   (procedural-expression ?exp2))
  ((verify-if-procedural-statement ?x)
   if
   (list ?X)
   (not (variable (first ?x)))
   (== ?x '(if ?sentence ?exp1 ))
   (sentence ?sentence)
   (procedural-expression ?exp1)
   ))

(def-relation cond-procedural-statement (?exp)
  :iff-def (and  (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp '(cond . ?clauses))
                 (every ?clauses cond-procedural-clause)))

(def-relation cond-procedural-clause (?clause)
  :iff-def (and (= ?clause (?sentence ?exp))
                (sentence ?sentence)
                (procedural-expression ?term)))


(def-relation in-env-procedural-statement (?exp)
  :iff-def (and  (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp '(in-environment ?env ?exp2))
                 (environment ?env)
                 (procedural-expression ?exp2)))

(def-relation DEFINED-PROCEDURAL-EXPRESSION (?x)
  :iff-def (and 
            (list ?x)
            (== ?x (?p . ?terms))
            (or (function ?p) (procedure ?p))
            (every ?terms procedural-expression)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-relation QUOTE-EXPRESSION (?exp)
  :iff-def (and (list ?exp)
                (not (variable (first ?exp)))
                (== ?exp '(quote ?term))
                (or (list ?term)
                    (term ?term))))

(def-relation EVAL-EXPRESSION (?exp)
  :iff-def (and (list ?exp)
                (not (variable (first ?exp)))
                (== ?exp '(eval ?term))
                (term ?term)))

(def-relation CALL-EXPRESSION (?exp)
  :iff-def (and (list ?exp)
                (not (variable (first ?exp)))
                (== ?exp '(call ?fun . ?args))
                (function ?fun)
                (every ?args term)))

(def-relation APPLY-EXPRESSION (?exp)
  :iff-def (and (list ?exp)
                (not (variable (first ?exp)))
                (== ?exp '(apply ?fun ?args))
                (function ?fun)
                (every ?args term)))


(def-relation findall-expression (?exp)
  :iff-def (and  (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp '(findall ?schema ?sentence))
                 (term-schema ?schema)
                 (sentence ?sentence)))


(def-relation setofall-expression (?exp)
  :iff-def (and  (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp '(setofall ?schema ?sentence))
                 (term-schema ?schema)
                 (sentence ?sentence)))

(def-relation the-expression (?exp)
  :iff-def (and  (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp '(the ?schema ?sentence))
                 (term-schema ?schema)
                 (sentence ?sentence)))

(def-relation TERM-SCHEMA (?thing)
  :iff-def (or (variable ?thing)
               (schema ?thing)))

(def-relation DEFINED-FUNCTION-EXPRESSION (?x)
  :iff-def (and
            (list ?x)
            (== ?x (?fun . ?terms))
            (function ?fun)
            (every ?terms term)))

(def-relation if-term (?exp)
  :iff-def (and (list ?exp)(verify-if-term ?exp)))

(def-rule verify-if-term 
  ((verify-if-term ?x)
   if
   (list ?x)
   (not (variable (first ?x)))
   (== ?x '(if ?sentence ?term1 ?term2))
   (sentence ?sentence)
   (term ?term1)
   (term ?term2))
  ((verify-if-term ?x)
   if
   (list ?x)
   (not (variable (first ?x)))
   (== ?x '(if ?sentence ?term1))
   (sentence ?sentence)
   (term ?term1)))

(def-relation cond-term (?exp)
  :iff-def (and  (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp '(cond . ?clauses))
                 (every ?clauses cond-functional-clause)))

(def-relation cond-functional-clause (?clause)
  :iff-def (and (== ?clause (?sentence ?term))
                (sentence ?sentence)
                (term ?term)))

(def-relation in-env-term (?exp)
  :iff-def (and  (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp '(in-environment ?env ?term))
                 (environment ?env)
                 (term ?term)))


(def-relation ENVIRONMENT (?exp)
  :iff-def (or (null ?exp)
               (and (list ?exp)
                    (not (variable (first ?exp)))
                    (== ?exp (?head . ?tail))
                    (binding ?head)
                    (environment ?tail))))


(def-relation BINDING (?thing)
  :iff-def (and  (list ?thing)
                 (== ?thing (?var . ?exp))
                 (variable ?var)
                 (not (member (namestring ?var)
                              (collect-vars-in-expression ?exp)))))
                 


(def-class CONSTANT (TERM)
  "A constant is a lisp symbol, which does not represent a variable"
  :lisp-fun #'(lambda (x env)
                (let ((x (instantiate x env)))
                  (if (and (symbolp x)
                           (not (variable? x)))
                    (list env)
                    :fail))))


(def-class GROUND-TERM (term) ?x
  "A term which does not contain variables (at all, not even quantified)"
  :iff-def (verify-ground-term ?X))

(def-rule verify-ground-term
  ((verify-ground-term ?x)
   if
   (atom ?x)
   (not (variable ?X)))
  ((verify-ground-term (?fun . ?terms))
   if
   (function ?fun)
   (every ?terms ground-term)))


(def-relation GROUND-THING (?thing)
  "Checks whether something contains variables")


(def-rule ground-thing
  ((ground-thing ?x)
   if
   (atom ?x)
   (not (variable ?X)))
  ((ground-thing ?x)
   if
   (list ?x)
   (== ?x (?head . ?tail))
   (ground-thing ?head)
   (ground-thing ?tail)))



(def-class SENTENCE (ocml-expression) ?x
   "The class of OCML sentences"
   :iff-def (or (basic-sentence ?x)
                (connected-sentence ?x)
                (quantified-sentence ?x)))

(def-class BASIC-SENTENCE (sentence) ?exp
   "The class of OCML basic sentences"
   :iff-def (and (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp (?rel . ?args))
                 (relation ?rel)
                 (every ?args legal-relation-argument)))

(def-class QUANTIFIED-SENTENCE (sentence) ?exp
   :iff-def (and (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp (?q ?schema ?sentence))
                 (member ?quantifier (exists forall))
                 (term-schema ?schema)
                 (sentence ?sentence)))


(def-class CONNECTED-SENTENCE (sentence) ?exp
  :iff-def (and (list ?exp)
                (not (variable (first ?exp)))
                (verify-connected-sentence ?exp)))


(def-rule VERIFY-CONNECTED-SENTENCE
  ((verify-connected-sentence  (?head . ?rest))
   if
   (member ?head (and or))
   (every ?rest sentence)
   )
  ((verify-connected-sentence  (not ?sentence))
   if
   (sentence ?sentence)
   )
  ((verify-connected-sentence  (?head ?exp1 ?exp2))
   if
   (member ?head '(=> <=>))
   (sentence ?exp1)
   (sentence ?exp2)))


(def-class ASSERTION () ?exp
   "A basic sentence with no variables."
   :iff-def (and (list ?exp)
                 (not (variable (first ?exp)))
                 (== ?exp (?rel . ?args))
                 (relation ?rel)
                 (every ?args legal-relation-argument)
                 (every ?args ground-thing)))
                 

(def-relation LEGAL-RELATION-ARGUMENT (?x)
  :iff-def (or (term ?x)(list ?x)))



(def-relation FREE-VAR-IN-SENTENCE (?var ?sentence)
  :iff-def (member (namestring  ?var)
                   (map namestring (all-free-vars-in-sentence ?sentence nil))))


(def-function ALL-FREE-VARS-IN-SENTENCE (?sentence ?bound-vars)
  :constraint (and (sentence ?sentence)
                   (every ?bound-vars meta-variable))
  :body (cond ((basic-sentence ?sentence)
               (collect-vars-in-rel-args (rest ?sentence) ?bound-vars))
              ((member (first ?sentence) '(and or not => <=>))
               (map denotation 
                    (apply union (map (lambda (?s)
                                    (map namestring 
                                         (all-free-vars-in-sentence ?s ?bound-vars)))
                                  (rest ?sentence)))))
              ((member (first ?sentence) '(exists forall))
               (in-environment 
                ((?bound-vars2 . (union (map namestring 
                                             (collect-variables (second ?sentence)))
                                        ?bound-vars)))
                (all-free-vars-in-sentence 
                 (third ?sentence) ?bound-vars2)))))


(def-function collect-vars-in-expression (?l)
  "Collect all variables which appear in the given input"
  :body (collect-vars-in-rel-args ?l nil))


(def-function collect-vars-in-rel-args (?args ?bvars) -> ?pair
  :body (if (null ?args)
          nil
          (map denotation 
               (union (map namestring (collect-vars-in-rel-arg (first ?args)  ?bvars))
                      (map namestring (collect-vars-in-rel-args (rest ?args)  ?bvars))))))

(def-function  collect-vars-in-rel-arg (?arg  ?bvars)
  :body (if (term ?arg)
          (all-free-vars-in-term ?arg ?bvars)
          (if (list ?arg)
            (collect-vars-in-rel-args ?arg ?bvars)
            nil)))

(def-function ALL-FREE-VARS-IN-TERM (?term ?bound-vars)
 
  :body (cond ((atom ?term)
               (if (variable ?term)
                 (if (member (namestring ?term)
                             ?bound-vars)
                   nil
                   (list-of (namestring ?term)))
                 nil))
              ((and (not (variable (first ?term)))
                         (member (first ?term) '(the setofall findall)))
               (in-environment 
                ((bound-vars2 . (union (map namestring
                                            (collect-variables  (second ?term))
                                       ?bound-vars))))
                (output 2)
                (all-free-vars-in-sentence (third ?term) ?bound-vars2)))

              ((== ?term '(if ?sentence ?term1 ?term2))
               (in-environment
                ((?vars .  (collect-variables ?sentence))
                 (?free-vars . (all-free-vars-in-sentence ?sentence ?bound-vars))
                 (?bound-vars1 . (union (difference 
                                          (map namestring ?vars)
                                          (map namestring ?free-vars))
                                        ?bound-vars))
                 (?free-vars2 . (all-free-vars-in-term ?term2 ?bound-vars))
                 (?free-vars1 . (all-free-vars-in-term ?term1 ?bound-vars1)))
                (map denotation 
                     (union  (map namestring ?free-vars1)
                             (map namestring ?free-vars2)
                             (map namestring ?free-vars)))))

              ((== ?term '(if ?sentence ?term1))
               (in-environment
                ((?vars . (collect-variables ?sentence))
                 (?free-vars . (all-free-vars-in-sentence ?sentence ?bound-vars))
                 (?bound-vars1 . (union (difference  (map namestring ?vars)
                                                     (map namestring ?free-vars))
                                        ?bound-vars))
                 (?free-vars1 . (all-free-vars-in-term ?term1 ?bound-vars1)))
                (map denotation (union (map namestring ?free-vars1)
                                       (map namestring ?free-vars)))))

              ((== ?term '(cond . ?clauses))
               (map (lambda (?clause)
                      (in-environment
                       ((?sentence . (first ?clause))
                        (?term2 . (second ?clause))
                        (?vars . (collect-variables ?sentence))
                        (?free-vars . (all-free-vars-in-sentence ?sentence ?bound-vars))
                        (?bound-vars1 . (union (difference  
                                                (map namestring ?vars)
                                                (map namestring ?free-vars))
                                               ?bound-vars))
                        (?free-vars1 . (all-free-vars-in-term ?term2 ?bound-vars1)))
                       (map denotation 
                            (union  (map namestring ?free-vars1)
                                    (map namestring ?free-vars)))))
                    ?clauses))
              
              ((== ?term '(in-environment ?bindings ?term2))
               (in-environment 
                ((?binding . (first ?bindings))
                 (?bvar . (namestring (first ?binding)))
                 (?bound-vars2 . (union (list-of ?bvar) ?bound-vars))
                 (?free-vars . (all-free-vars-in-term (second ?binding)
                                                      ?bound-vars2)))
                (map denotation 
                (union (map namestring ?free-vars)
                        (map namestring
                       (if (null (rest ?bindings))
                         (all-free-vars-in-term ?term2
                                                ?bound-vars2)
                         (all-free-vars-in-term
                          (list-of in-environment
                                   (rest ?bindings)
                                   ?term2)
                          ?bound-vars2)))))))
              ((true)
               ;(if (> (length ?term) 1)
               ; (append (all-free-vars-in-term (second ?term)
               ;                                ?bound-vars)
               ;          (all-free-vars-in-term (rest (rest ?term))
               ;                                 ?bound-vars))
               ;  nil))))
                         
               (map (lambda (?x)
                      (all-free-vars-in-term ?x ?bound-vars))
                    ?term))))

                              

(def-function COLLECT-VARIABLES (?expression)
  :body (if (atom ?expression)
          (if (variable ?expression)
            (list-of ?expression)
            nil)
          (union (collect-variables (first ?expression))
                 (collect-variables (rest ?expression)))))

(def-function RENAME-VARIABLES (?schema)
  :body (map new-var ?schema))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-class RULE ())


(def-class BACKWARD-RULE (rule)
  ((has-clause-list :type bc-clause-list :min-cardinality 1 :max-cardinality 1)))

(def-class bc-clause-list () ?x
  :iff-def (and (list ?X)
                (every ?x backward-clause)))

(def-class BACKWARD-CLAUSE (list) ?x
  :iff-def (and (list ?x)
                (not (variable (second ?x)))
                (== ?x (?conse if . ?ante))
                (basic-sentence ?conse)
                (every ?ante sentence))
  :avoid-infinite-loop t)

(def-class PROOF-EXPRESSION (backward-clause) ?x
  ((proves-relation :type relation))
  :constraint (and 
                   (== ?x (?tail if . ?rest))
                   (== ?tail (?rel . ?args))
                   (= ?rel (the ?rel2 (proves-relation ?X ?rel2)))))
  

(def-relation PROVES (?brule ?goal)
  "Checks that ?brule can prove ?goal.
   Example: (tell (foo a b))
            (ask (proves '((foo ?y ?x) if (foo ?x ?y)) 
                         '(foo b a)))
            Solution: ((PROVES '((FOO B A) IF (FOO A B)) '(FOO B A)))"
  :iff-def (or (and (== ?brule (?tail if . ?rest))
                    (= ?tail ?goal)
                    (goal-holds (cons 'and ?rest)))
                (and (== ?brule (?tail))
                     (= ?tail ?goal))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The definitions below allow an alternative, 'metalinguistic' treatment of 
;;;meta expressions.  The idea is to use meta-objects to talk about the
;;;properties of OCML expressions. A meta-object is an object which refers
;;;to an OCML expression. The meta-object M denoting an object D is constructed
;;;by enclosing D within strings.  For instance "(foo a b)" is the meta-object
;;;which denotes the expression (foo a b).  Sometimes it is useful to describe 
;;;all the costituents of an expression.  For this reason we also introduce 
;;;an alternative representation, in which each meta-object is decomposed into  
;;;its (meta-)constituents.  For instance, an alternative representation of
;;;"(foo a b)" is ("foo" "a" "b").  The advantages of this representation is that 
;;;it allows us to intermix domain and meta objects - e.g. ("foo" "a" ?x).


(def-class META-OBJECT () (?x)
  "This relation is true if ?x is a meta-object.
   A meta-object is a reference to an OCML object.
   The purpose of meta objects is to be able to 
   reason about OCML expressions with variables
   There are two types of meta-objects:  normal meta-objects
   and decomposed meta-objects.  Normal meta-objects are just
   strings which enclose legal OCML expressions.
   For instance ''(foo a b)'' is the meta-referent of 
   the sentence (foo a b).
   Decomposed meta objects are OCML expressions in which
   every element has been replaced by its referent, 
   e.g. (''foo'' ''a'' ''b'').  Decomposed meta objects are needed to 
   analyse the structure of an OCML expression, without getting
   into trouble with variables")


(def-rule meta-object
  ((meta-object ?x)
   if
   (normal-meta-object ?x))
  ((meta-object ?x)
   if
   (decomposed-meta-object ?x)))



(def-relation NORMAL-META-OBJECT (?x)
  :iff-def (and (string ?x)
                (ocml-expression (denotation ?x))))

(def-relation DECOMPOSED-META-OBJECT (?x)
  :iff-def (ocml-expression (denotation ?x)))

(def-function NAMESTRING (?x)
  "This returns the meta object corresponding to 
   an OCML expression"
  :lisp-fun #'meta-object)

(def-function DENOTATION (?x)
  "This returns the denotation of a meta object.
   ?x can be either decomposed or not.  This function 
   returns :nothing if ?x is not a meta-object"
  :constraint (meta-object ?X)
  :lisp-fun #'denotation)

(def-relation META-VARIABLE (?thing)
  
  :iff-def (and (string ?thing)
                (variable (denotation ?thing)))
  :lisp-fun #'(lambda (x env)
                (let ((y (lookup-or-self x env)))
                  (if
                    (and (string y)
                         (variable? (denotation y)))
                    (list env)
                    :fail))))

(def-class META-KAPPA-EXPRESSION (meta-object) ?thing
    :iff-def (and (string ?thing)
                  (kappa-expression (denotation ?thing))))

(def-class META-PROOF-EXPRESSION (meta-object) ?thing
    :iff-def (and (string ?thing)
                  (proof-expression (denotation ?thing))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-function META-DECOMPOSE (?meta-object)
  "Takes a meta-object and decompose it into its
   meta-constituents"
  :constraint (normal-meta-object ?meta-object)
  :lisp-fun #'decompose-meta-object)

(def-function META-RECOMPOSE (?meta-object)
  "Recomposes a decomposed meta-object"
  :constraint (decomposed-meta-object ?meta-object)
  :lisp-fun #'recompose-meta-object)


(def-relation DENOTES-BASIC-SENTENCE (?meta-object)
  :iff-def (and (meta-object ?meta-object)
                (basic-sentence (denotation ?meta-object))))

(def-relation DECOMPOSED-META-SENTENCE (?meta-object)
  :iff-def (and (decomposed-meta-object ?meta-object)
                (sentence (denotation ?meta-object))))

(def-relation DECOMPOSED-META-TERM (?meta-object)
  :iff-def (and (decomposed-meta-object ?meta-object)
                (term (denotation ?meta-object))))

(def-relation DECOMPOSED-META-LIST (?meta-object)
  :iff-def (and (decomposed-meta-object ?meta-object)
                (list (denotation ?meta-object))))


(def-relation FREE-VAR-IN-META-SENTENCE (?meta-var ?meta-sentence)
  :iff-def (free-var-in-sentence (denotation ?meta-var)
                                 (denotation ?meta-sentence)))


(def-function ALL-FREE-VARS-IN-META-SENTENCE (?meta-sentence ?bound-vars)
  :constraint (and (sentence (denotation ?meta-sentence))
                   (every ?bound-vars meta-variable))
  :body (all-free-vars-in-sentence (denotation ?meta-sentence)
                                   (map denotation ?bound-vars)))

(def-function ALL-FREE-VARS-IN-META-TERM (?meta-term ?bound-vars)
  :constraint (and (term (denotation ?meta-term))
                   (every ?bound-vars meta-variable))
  :body (all-free-vars-in-term (denotation ?meta-term)
                               (map denotation ?bound-vars)))



(def-function COLLECT-META-VARIABLES (?meta-expression)
  :body (if (atom ?meta-expression)
          (if (meta-variable ?meta-expression)
            (list-of ?meta-expression)
            nil)
          (union (collect-meta-variables (first ?meta-expression))
                 (collect-meta-variables (rest ?meta-expression)))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;(def-function EVAL (?form)
;;;;  "Calls the ocml evaluator to evaluate a function or
;;;;  procedural expression"
;;;  :lisp-fun #'ocml-eval-gen)
              

;;(def-function PROCEDURE-EVAL (?form)
;;  "Calls the ocml evaluator to evaluate a 
;;   procedural expression"
;;  :lisp-fun #'procedure-eval)
              
               