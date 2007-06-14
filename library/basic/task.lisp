;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

(def-class task () ?task
  "An OCML task is characterised by its
   input roles, output role, goal, and 
   (optional) body. The goal expression is a kappa 
   expression which takes as argument the task itself.
   A role is a slot of a task"
  ((has-input-role :type role)
   (has-output-role :type role)
   (has-goal-expression :type legal-task-goal-expression)
   (has-body :type unary-procedure))
  :constraint (<=> (has-role ?task ?role)
                   (slot-of ?role ?task))
  :lisp-class-name task)

(def-class TASK-TYPE () ?x
  :iff-def (and (class ?x)
                (subclass-of ?x task)))


(def-class legal-task-goal-expression (kappa-expression) ?exp
  :iff-def (and (kappa-expression ?exp)
                (= (arity ?exp) 1)
                (= ?exp (kappa ?schema ?sent))
                (= ?vars (all-free-vars-in-sentence ?sent))
                (= (length ?vars) 1)
                (= (namestring (first ?schema)) (namestring (first ?vars)))))


(def-relation HAS-ROLE (?task ?role)
  :iff-def (member ?role 
                   (union (all-class-slot-values has-input-role ?task)
                          (all-class-slot-values has-output-role ?task))))

                          
(def-function TASK-ROLES (?task) -> ?roles
  :constraint (and (task ?task)
                   (every ?roles role))
  :body (setofall ?x (has-role ?task ?x)))


;;;ACHIEVED ---
(def-relation achieved (?task-inst)
  "This checks whether the goal of an individual
   task has been achieved"
  :iff-def (holds (the ?exp (has-goal-expression ?task-inst ?exp))
                  ?task-inst))



(def-class ROLE  (slot) ?role
  "A role is a binary relation associated with a task by means
   of the 'has-role' relation"
  :iff-def (exists ?c 
                   (and (task ?c)
                        (has-role ?c ?role))))


(def-class PROBLEM-SOLVING-METHOD (task))

(def-class primitive-problem-solving-method
  (problem-solving-method)
  ((has-body :min-cardinality 1)))

(def-class TASK-DECOMPOSITION-METHOD (problem-solving-method)
  ((has-subtask :min-cardinality 1 :type task-type)))


(def-class APPLICATION-DOMAIN ())

;;;APPLICATION
(def-class application ()
  ((tackles-domain :type application-domain)
   (uses-method :type problem-solving-method)
   (tackles-task :type task))
  :lisp-class-name application)
