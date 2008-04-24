;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

(def-relation HAS-INPUT-ROLE (?thing ?role)
  "This definition generalises the notion of 
   'having an input role' to classes as well 
    as tasks instances.  If ?class is a method, then
    it also 'inherits the input roles from the task type
    to which it is applicable"
  :sufficient (or (and (instance ?thing)
                       (has-input-role (the-parent ?thing) ?role))
                  (and (class ?thing)
                       (subclass-of ?thing task)
                       (or ;;;;;;;(and (slot-of has-input-role ?thing)
                        (member ?role (all-class-slot-values 
                                       ?thing has-input-role))
                        (and (subclass-of ?thing problem-solving-method)
                             (tackles-task-type ?thing ?task-type)
                             ;; (member ?task-type (all-class-slot-values 
                             ;;                     ?thing tackles-task-type))
                             (has-input-role ?task-type ?role))))))

(def-relation HAS-OUTPUT-ROLE (?thing ?role)
  "This definition generalises the notion of 
   'having an output role' to classes as well 
    as tasks instances.  If ?thing is a method, then
    it also 'inherits the output role from the task type
    to which it is applicable"
  :sufficient  (or (and (instance ?thing)
                        (has-output-role (the-parent ?thing) ?role))
                   (and (class ?thing)
                        (subclass-of ?thing task)
                        (or ;;;;;;(and (slot-of has-output-role ?thing)
                         (member ?role (all-class-slot-values 
                                        ?thing has-output-role))
                         (and (subclass-of ?thing problem-solving-method)
                              (tackles-task-type ?thing ?task-type)
                              ;;(member ?task-type (all-class-slot-values 
                              ;;                   ?thing tackles-task-type))
                              (has-output-role ?task-type ?role))))))

(def-relation HAS-CONTROL-ROLE (?thing ?role)
  :sufficient (or (and (instance-of ?thing composite-task)
                       (has-control-role (the-parent ?thing) ?role))
                  (and (subclass-of ?thing composite-task)
                       (or 
                        (member ?role (all-class-slot-values 
                                       ?thing has-control-role))
                        (and (has-generic-subtasks ?thing ?subs)
                             (member ?sub ?subs)
                             (has-output-role ?sub ?role))))))

;;;;;;;;;;;;;;;;;
;;;the next two  come from John....
(def-relation has-local-input-role (?thing ?role)
  "This definition is the same as has-input-role except that we no
longer check for the input roles associated with any psm's task."
  :sufficient (or (and (instance ?thing)
                       (has-input-role (the-parent ?thing) ?role))
                  (and (class ?thing)
                       (subclass-of ?thing task)
                       (member ?role (all-class-slot-local-values 
                                       ?thing has-input-role)))))

(def-relation has-local-output-role (?thing ?role)
  "This definition is the same as has-output-role except that we no
longer check for the input roles associated with any psm's task."
  :sufficient  (or (and (instance ?thing)
                        (has-output-role (the-parent ?thing) ?role))
                   (and (class ?thing)
                        (subclass-of ?thing task)
                        (member ?role (all-class-slot-local-values 
                                        ?thing has-output-role)))))

;;;;;;;;;;;;;;;;





(def-class  task () ?task
  "an ocml task is characterised by its
   input roles, output role, and goal. the goal expression is a  
   kappa expression which takes as argument the task itself and a 
   value (which is meant to be a possible result from carrying out the 
   task.  the goal is satisfied if the kappa expression holds for its
   two arguments. 
   a role is a slot of a task.
   tasks divide into two main subclasses: 
   goal-specification-task and executable-task.   the former
   provides only a goal specification, while the latter provides
   also an 'organic' method for achieving the task"
  ((has-input-role :type role)
   (has-output-role :type role)
   (has-precondition :type unary-kappa-expression)
   (has-assumption :type unary-kappa-expression)
   (has-goal-expression :type legal-task-goal-expression))
  :constraint (forall (?role)
                      (=> (has-role ?task ?role)
                          (and (functional-relation ?role)
                               (or (slot-of ?role (the-parent ?task))
                                   (and (problem-solving-method ?task)
                                        (tackles-task ?task ?task2)
                                        (slot-of ?role (the-parent ?task2)))
                                   (has-control-role ?task ?role)))))
  
  
  :sufficient-for-type-checking (forall (?role)
                                             (=> (has-role ?task ?role)
                                                 ;;;(and (functional-relation ?role)
                                                 (or (slot-of ?role (the-parent ?task))
                                                     (and (problem-solving-method ?task)
                                                          (tackles-task-type (the-parent ?task) ?task2-parent)
                                                          (slot-of ?role  ?task2-parent))
                                                     (has-control-role ?task ?role))))
  
  :lisp-class-name task)

(def-axiom TASKS-ARE-EITHER-GOAL-SPEC-OR-EXECUTABLE
 (exhaustive-subclass-partition 
              task
              (set-of goal-specification-task
                      executable-task)))


(def-class TASK-TYPE () ?x
  :iff-def (subclass-of ?x task))


(def-class LEGAL-TASK-GOAL-EXPRESSION (kappa-expression) ?exp
  "A task goal expression is a kappa expression with arity 2, 
   which does not contain free variables.  The first argument to 
   the kappa expression represents a task-instance, the second
   the result of the task"
  :constraint  (and (== ?exp (kappa ?schema ?sent))
                    (schema ?schema)
                    (sentence ?sent))
  :sufficient-for-type-checking (and (== ?exp (kappa ?schema ?sent))
                    (schema ?schema)
                    (sentence ?sent)))

             ;;;(and ;;;(= (arity ?exp) 2)
              ;;;;  (== ?exp (kappa ?schema ?sent))
             ;;;;   (= nil (all-free-vars-in-sentence ?sent (map namestring ?schema)))))
              



(def-class GOAL-SPECIFICATION-TASK (task) ?task
  "A goal-specification-task is a task with a goal 
   expression and no body"
  ((has-goal-expression :cardinality 1))
  :constraint (not (exists ?body
                           (has-body ?task ?body))))


(def-class PROBLEM-TYPE () ?c
  "A problem type is a goal specification task which is not a subtask of a 
   method - i.e. it defines the root of a task-method decomposition.
   Conceptually, a problem type defines a generic class of 
   applications - e.g. parametric design"
  :iff-def (and (subclass-of ?c goal-specification-task)
                (not (exists ?m 
                             (and 
                              (has-generic-subtasks ?m ?subs)
                              (member ?c ?subs))))))

(def-class EXECUTABLE-TASK (task)
  "An executable task is a task with a body - i.e. a 
   task whose specification also includes a mechanism for 
   achieving it"
  ((has-body :type unary-procedure)
   (has-control-role :type role))
 
  :lisp-class-name executable-task)

(def-axiom executable-tasks-are-either-primitive-or-composite
  (exhaustive-subclass-partition 
              executable-task
              (set-of primitive-task
                      composite-task)))


(def-class COMPOSITE-TASK (executable-task) ?task
  "A composite task is a task which introduces a subtask
   decomposition.  Something is an instance of this task if
   its parent introduces a generic task-subtask
   decomposition"
  :iff-def (and (has-generic-subtasks ?c ?subs)
                (direct-instance-of ?task ?c)))

             

(def-relation HAS-GENERIC-SUBTASKS (?task-type ?subs)
  "Use this to model generic task-subtask decompositions"
  :constraint (and (subclass-of ?task-type composite-task)
                   (every ?subs task-type)))





(def-class PRIMITIVE-TASK (executable-task) ?task
  "An executable task which is not a composite task"
  :iff-def (and (executable-task ?task)
                        (not (composite-task ?task)))
  :no-proofs-by (:iff-def))


(def-class ROLE  (slot) ?role
  "A role is a binary relation associated with a task by 
   means of the 'has-role' relation.  The value cardinality   
   of a role-defining slot is 1."
  :constraint (forall (?i)
                      (=> (and (has-role ?class ?role)
                               (instance-of ?i ?class))
                          (has-one ?i ?role)))
  :iff-def (exists ?c 
                   (and (task-type ?c)
                        (has-role ?c ?role))))
                        
  
(def-relation HAS-ROLE (?thing ?role)
  "Generalises from input output and control roles"
  :iff-def (or (has-input-role ?thing ?role)
               (has-control-role ?thing ?role)
               (has-output-role ?thing ?role)))


(def-function TASK-ROLES (?class) -> ?roles
  :constraint (and (task-type ?class)
                   (every ?roles role))
  :body (setofall ?x (has-role ?class ?x)))


(def-function LOCAL-ROLE-VALUE (?task ?role)
  
  "The local value of a role, say ?role, in a task, say ?task, 
   is defined as the value of the slot ?role in ?task.  
   If the max cardinality of ?role in ?task is either 
   undefined or defined and greater than 1, then the set of all 
   slot values are returned.  Otherwise only one is returned"
  
  :body (the ?v (holds ?role ?task ?v)))



(def-function ROLE-VALUE (?task ?role)
  "The value of a role is its local value if it exists.  
   If it does not then the subtask-of hierachy is searched 
   for a value."
  :body (in-environment ((?value . (local-role-value 
                                    ?task ?role)))
           (if (and (= ?value :nothing)
                    (subtask-of ?task ?supertask))
             (role-value ?supertask ?role)
             ?value)))


(def-procedure SET-ROLE-VALUE (?task ?role ?value)
  :body (set-slot-value ?task ?role ?value))
          
 

(def-procedure ACHIEVE-GENERIC-SUBTASK (?supertask 
                                        ?task-type 
                                        &rest ?actual-role-pairs)
  :body (in-environment
         ((?name . (new-symbol ?task-type)))
         ;;(break)
         (tell (append (list-of ?task-type ?name) 
                       ?actual-role-pairs))
         (tell (subtask-of ?name ?supertask))
         (solve-task ?name)))

(def-procedure INSTANTIATE-GENERIC-SUBTASK 
  (?supertask ?task-type &rest ?actual-role-pairs)
  :body (in-environment 
         ((?name . (new-symbol ?task-type)))
         (tell (append (list-of ?task-type ?name) 
                       ?actual-role-pairs))
         (tell (subtask-of ?name ?supertask))
         ?name))

  
;(def-relation ACHIEVED (?task-inst ?goal-args)
;  "A task has been achieved if its goal holds in the current model.
;   A method has been achieved either if its goal has been achieved or
;   if its associated task has."
;  :iff-def (or (and (has-goal-expression ?task-inst ?exp)
;                   (goal-holds (append (list-of holds ?exp)
;                                        (cons ?task-inst ?goal-args))))
;               (and (problem-solving-method ?task-inst)
;                    (tackles-task ?task-inst ?task-inst2)
;                    (achieved ?task-inst2 ?goal-args))))

(def-relation ACHIEVED (?task-inst ?result)
  "A task has been achieved if its goal holds in the current model.
   A method has been achieved either if its goal has been achieved or
   if its associated task has."
  :iff-def (or (and (= ?exp (role-value ?task-inst has-goal-expression))
                    (holds ?exp ?task-inst ?result))
               (and (problem-solving-method ?task-inst)
                    (tackles-task ?task-inst ?task-inst2)
                    (achieved ?task-inst2 ?result))))

(def-relation SATISFIES-TASK-GOAL (?task-inst ?result)
  :iff-def (and (= ?exp (role-value ?task-inst has-goal-expression))
                    (holds ?exp ?task-inst ?result)))
                    
(def-class LEGAL-POSTCONDITION (kappa-expression) ?exp
  "A legal PSM postcondition is a kappa expression with arity 2, 
   which does not contain free variables.  The first argument to 
   the kappa expression represents a psm-instance, the second
   the result of the psm"
  :constraint  (and (== ?exp (kappa ?schema ?sent))
                    (schema ?schema)
                    (sentence ?sent))
  :sufficient-for-type-checking (and (== ?exp (kappa ?schema ?sent))
                    (schema ?schema)
                    (sentence ?sent)))



(def-class PROBLEM-SOLVING-METHOD (executable-task)
  "A problem solving method is an executable task which 
   is associated with (tackles) a class of tasks. 
   The slot has-output-mapping specifies a function which maps
   the result returned by the method to a task.
   The reason for this mapping is to allow the decoupling
   of the type of result returned by the method from that expected
   by the task.  This provides greater flexibility and also makes it 
   possible to specify solution conditions for a method which use 
   different types of output from that used by a task"
  ((tackles-task :type goal-specification-task)
   (has-postcondition :type legal-postcondition)
   (has-output-mapping 
    :default-value '(lambda (?psm ?result)
                      ?result)))
  :lisp-class-name problem-solving-method)


(def-relation TACKLES-TASK-TYPE (?method-class ?task-type)
  "This relation provides a fairly coarse-grained indexing of the 
   library: each method is associated to a class of tasks to which
   it can be applied"
  :constraint (and (subclass-of ?method-class problem-solving-method)
                   (task-type ?task-type))
  :sufficient (and ;;;;(class ?method-class)
                            (superclass-of  ?super ?method-class)
                            (tackles-task-type ?super ?task-type))
  :no-proofs-by (:sufficient))

(def-rule infer-tackles-task-type 
  ((tackles-task-type ?method-class ?task-type)
   if
   (and (asserted (tackles-task-type ?super ?task-type))
        (superclass-of  ?super ?method-class))))


(def-relation APPLICABILITY-CONDITION (?method-class ?exp)
  "This relation provides a more fine-grained test to check
   the applicability of a class of methods to a specific task"
  :constraint (and (subclass-of ?method-class problem-solving-method)
                   (unary-relation ?exp)))

(def-relation APPLICABLE-TO-TASK (?method-class ?task-inst)
  :iff-def (or (not (applicability-condition ?method-class  ?exp))
               (and (applicability-condition ?method-class  ?exp)
                    (holds ?exp  ?task-inst))))
                   

(def-class PRIMITIVE-METHOD (problem-solving-method primitive-task) ?x
  "A method with no subtasks"
  :iff-def (and (problem-solving-method ?x)
                (not (exists ?subs (has-generic-subtasks (the-parent ?x) ?subs))))
  :avoid-infinite-loop t)

(def-class DECOMPOSITION-METHOD (problem-solving-method composite-task) ?x
  "A method with subtasks"
  :iff-def (exists ?subs (has-generic-subtasks (the-parent ?x) ?subs)))

;(def-procedure SOLVE-TASK (?task-instance)
;  "A task, ?task, is executed by evaluating its body 
;   in an environemnt in which the schema of the class
;   corresponding to the parent of ?task is bound to
;   ?task." 
;  :body (if (executable-task ?task-instance has-body ?body)
;          (execute-task-body ?body ?task-instance) 
;          (if (and (subclass-of ?psm-type problem-solving-method)
;                   (tackles-task-type ?psm-type  ?c)
;                   (instance-of ?task-instance ?c)
;                   (applicable-to-task ?psm-type ?task-instance))
;            (in-environment 
;             ((?method . (instantiate-generic-subtask 
;                          ?task-instance ?psm-type)))
;             (apply-method-to-task ?method ?task-instance)))))

(def-procedure SOLVE-TASK (?task-instance)
  "A task, ?task, is executed by evaluating its body 
   in an environment in which the schema of the class
   corresponding to the parent of ?task is bound to
   ?task." 
  :body (if (executable-task ?task-instance has-body ?body)
          (execute-task-body ?body ?task-instance) 
           (in-environment 
            ((?best-psm . (choose-best-method-class
                  (setofall ?psm-type
                            (and 
                             (subclass-of ?psm-type
                                          problem-solving-method)
                             (instance-of ?task-instance ?c)
                             (tackles-task-type ?psm-type  ?c)
                             (applicable-to-task ?psm-type ?task-instance))))))
            (if  (= ?best-psm :nothing)
              (output "cannot find a psm to solve task ~s"
                      ?task-instance)
              (in-environment 
             ((?method . (instantiate-generic-subtask 
                          ?task-instance ?best-psm)))
             (apply-method-to-task ?method ?task-instance))))))


(def-function CHOOSE-BEST-METHOD-CLASS (?psm-types)
  :body (if (null ?psm-types)
          :nothing
          (if (= (length ?psm-types) 1)
            (first ?psm-types)
            (if (exists ?x
                        (and 
                         (member ?x ?psm-types)
                         (use-method ?x ?c ?m)))
              (choose-from-use-method-statements ?psm-types)
              (first ?psm-types)))))

(def-function CHOOSE-FROM-USE-METHOD-STATEMENTS (?psm-types)
  :body (if (and (use-method ?x ?c (the-current-task))
                 (member ?x ?psm-types))
          ?x
          (in-environment 
           ;;try to pick the most specific use-method
           ;;statement for this subtask
           ((?psm-type . (the ?x 
                           (and 
                            (member ?x ?psm-types)
                            (use-method ?x ?c ?m)
                            (instance-of (the-current-method) ?m)
                            (not 
                             (exists 
                              (?m2 ?x2)
                              (and (member ?x2 ?psm-types)
                                   (use-method ?x2 ?c ?m2)
                                   (subclass-of ?m2 ?m)
                                   (instance-of 
                                    (the-current-method) 
                                    ?m2))))))))
           (if (= ?psm-type :nothing)
             (first ?psm-types)
             ?psm-type))))
                                     

(def-relation use-method (?sub-method ?sub-task ?thing)
  "Use instances of this relation to specify which sub-method
   to use when solving a generic subtask of a problem. The third
   argument can be used to contextualise this statement within a 
   problem solving method or a particular problem.
   EXAMPLE: (use-method HC-CONTROL DESIGN-FROM-STATE HC-DESIGN)"
  :constraint (and (subclass-of ?sub-method problems-solving-method)
                   (subclass-of ?sub-task task)
                   (or (task ?thing)
                       (subclass-of ?thing problem-solving-method))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;new stuff to handle IRS - enrico 22/8/02

(def-function find-all-suitable-psms (?task-type ?task-instance)
  :body (in-environment 
         ((?structs . (all-subclass-structures-in-all-ontologies 'problem-solving-method)))
         (setofall ?s
                   (and 
                    (member ?s ?structs)
                    (= (the-name-of ?s) ?psm-type)
                    (holds-in-ontology (home-ontology-of-structure ?s)
                                       (and 
                                        (tackles-task-type ?psm-type  ?task-type)
                                        (applicable-to-task ?psm-type ?task-instance)))))))

(def-procedure SOLVE-TASK-GENERALIZED (?task-instance)
  "A task, ?task, is executed by evaluating its body 
   in an environment in which the schema of the class
   corresponding to the parent of ?task is bound to
   ?task." 
  :body (if (executable-task ?task-instance has-body ?body)
          (execute-task-body ?body ?task-instance) 
          (in-environment 
           ((?c . (the ?classb (instance-of ?task-instance ?classb)))
            (?best-psm . (first (find-all-suitable-psms ?c ?task-instance))))
           (if  (= ?best-psm :nothing)
             (output "cannot find a psm to solve task ~s"
                     ?task-instance)
             (in-ontology (home-ontology-of-structure ?best-psm)
                          (do
                              (output "~s" (home-ontology-of-structure ?best-psm))
                          (in-environment 
                           ((?method . (instantiate-generic-subtask 
                                        ?task-instance (the-name-of ?best-psm))))
                           (apply-method-to-task ?method ?task-instance))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



                       

(def-procedure PERFORM-EXECUTABLE-TASK (?task-instance)
  :body (if (has-body ?task-instance ?body)
          (execute-task-body ?body ?task-instance)))

;;This is the same as PERFORM-EXECUTABLE-TASK..here only for compatibility with 
;;;previous versions of teh ontology
;;(def-procedure EXECUTE-PRIMITIVE-TASK (?task-instance)
;;  :body (if (has-body ?task-instance ?body)
;;          (execute-task-body ?body ?task-instance)))


(def-procedure EXECUTE-PRIMITIVE-TASK (?task-instance)
  "Now calls a lisp function so that it can be compatible with 
both local calls in the OCML environment and distributed calls 
from the IRS"
  :lisp-fun #'(lambda (x) (execute-primitive-task x)))



(def-procedure EXECUTE-TASK-BODY (?body ?task-instance)
  :body (call ?body ?task-instance)
  :lisp-fun #'execute-task-body)


(def-relation SUBTASK-OF (?inst1 ?inst2)
  "This relation is used to model the specific
   task-subtask hierarchy constructed at 
   execution time"
  :constraint (and (task ?inst1) ;;;;(executable-task ?inst1)
                   (task ?inst2))
  :sufficient (and (problem-solving-method ?inst1)
                   (tackles-task ?inst1 ?inst2)))


(def-class APPLICATION-DOMAIN ())


;;;APPLICATION
(def-class application ()
  ((tackles-domain :type application-domain)
   (uses-method :type problem-solving-method)
   (tackles-task :type goal-specification-task))
  :lisp-class-name application)


(def-procedure SOLVE-APPLICATION (?appl)
  :body (if (application ?appl
                         uses-method ?method-inst
                         tackles-task ?task-inst)
          (do
            (unassert (current-application ?any))
            (tell (current-application ?appl))
            (apply-method-to-task ?method-inst ?task-inst))))

(def-function the-current-task ()
  :body (if (current-application ?appl)
          (the ?x (tackles-task ?appl ?x))))

(def-function the-current-method ()
  :body (if (current-application ?appl)
          (the ?x (uses-method ?appl ?x))))

  
(def-procedure ACHIEVE-TOP-LEVEL-TASK (?task-type &rest ?actual-role-pairs)
  :body (in-environment ((?name . (new-symbol ?task-type)))
         (tell (append (list-of ?task-type ?name) 
                       ?actual-role-pairs))
         
         (solve-task ?name)))

(def-procedure APPLY-METHOD-TO-TASK (?method-inst ?task-inst)
  :body (do 
          (tell (tackles-task ?method-inst ?task-inst))
          (in-environment
           ((?output-role . (the-slot-value 
                             ?task-inst 
                             has-output-role))
            (?fun . (the ?fun (has-output-mapping ?method-inst ?fun)))
            (?result . (execute-primitive-task ?method-inst)))
           (set-slot-value ?task-inst 
                           ?output-role  
                           (call ?fun ?method-inst ?result))
           ?result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;NEW STUFF FOR IRS-II
(def-procedure ACHIEVE-GENERIC-TASK (?task-type  ?actual-role-pairs)
  :body (in-environment 
         ((?pair . (select-suitable-psm 
                        ?task-type ?actual-role-pairs 
                        (find-all-psms-tackling-task-type 
                         ?task-type))))
         
         (if  (= ?pair :nothing)
           (output "cannot find a psm to achieve task ~s"
                   ?task-type)
           
           (apply-method-to-task 
            (second ?pair)(first ?pair)))))


;;;SELECT-SUITABLE-PSM
;;;psms is a list of psm structures
(defun select-suitable-psm (task-type actual-role-pairs psms)
  (loop for psm in psms
        for flag = (suitable-psm? task-type actual-role-pairs psm)
        until flag
        finally
        (return (or flag
                  :nothing))))

;;;FUNCTION SELECT-SUITABLE-PSM
(def-function select-suitable-psm (?task-type ?actual-role-pairs ?psms)
  :lisp-fun #'(lambda (x y z) (select-suitable-psm x y z)))



;(defun suitable-psm? (task-type actual-role-pairs psm)
;  (unless (member (home-ontology psm) *current-ontologies*)
;    (switch-to-ontology (home-ontology psm)))
;  (let ((inst (new-instance task-type actual-role-pairs)))
;    (when (holds? 'APPLICABLE-TO-TASK (name psm)
;                  inst)
;      (list inst
;            (ocml-eval-gen `(instantiate-generic-subtask 
;                             ,(name inst)
;                             ,(name psm)))))))



;;modified by john
(defun suitable-psm? (task-type actual-role-pairs psm)
  (let ((original-ontology *current-ontology*))
    (unwind-protect
        (progn 
          (switch-to-ontology (home-ontology psm))
          (let ((inst (new-instance task-type actual-role-pairs)))
            (when (holds? 'APPLICABLE-TO-TASK (name psm)
                          (name inst))
              (list inst
                    (ocml-eval-gen `(instantiate-generic-subtask 
                                     ,(name inst)
                                     ,(name psm)))))))
      (switch-to-ontology original-ontology))))



  
(def-procedure find-all-psms-tackling-task-type (?task-type ?ontology)
  :lisp-fun #'find-all-psms-tackling-task-type)


;;;;;;;;;More stuff from John;;;;;;;;;;;;;;;
(def-relation all-task-input-roles (?task ?roles)
  "This is a much faster version of has-input-role. Has-input-role can
sometimes take over 10 seconds with setofall."
  :sufficient (= ?roles (all-class-slot-values ?task has-input-role)))

(def-relation all-task-output-roles (?task ?roles)
  "This is a much faster version of has-output-role. Has-input-role can
sometimes take over 10 seconds with setofall."
  :sufficient (= ?roles (all-class-slot-values ?task has-output-role)))


(def-relation all-psm-input-roles (?psm ?roles)
  "This is a much faster version of has-input-role. Has-input-role can
sometimes take over 10 seconds with setofall."
  :sufficient (or (and (tackles-task-type ?psm ?task-type)
                       (= ?roles (remove-duplicates
                                  (append (all-class-slot-values
                                           ?task-type has-input-role)
                                          (all-class-slot-values ?psm
                                                                 has-input-role)))))
                  (= ?roles (all-class-slot-values ?psm
                                                   has-input-role))))

(def-relation all-psm-output-roles (?psm ?roles)
  "This is a much faster version of has-output-role. Has-output-role can
sometimes take over 10 seconds with setofall."
  :sufficient (or (and (tackles-task-type ?psm ?task-type)
                       (= ?roles (remove-duplicates
                                  (append (all-class-slot-values
                                           ?task-type has-output-role)
                                          (all-class-slot-values ?psm
                                                                 has-output-role)))))
                  (= ?roles (all-class-slot-values ?psm
                                                   has-output-role))))


(def-relation all-local-input-roles (?task ?roles)
  "This is a much faster version of has-input-role. Has-input-role can
sometimes take over 10 seconds with setofall."
  :sufficient (= ?roles (all-class-slot-local-values ?task
                                                     has-input-role)))

(def-relation all-local-output-roles (?task ?roles)
  "This is a much faster version of has-output-role. Has-output-role can
sometimes take over 10 seconds with setofall."
  :sufficient (= ?roles (all-class-slot-local-values ?task
                                                     has-output-role)))



















