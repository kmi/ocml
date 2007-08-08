;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defun spying? (pred)
  (or (eq  *spied-predicates* :all)
      (member pred *spied-predicates*)))

;(defun clear-spy-points ()
;  (setf  *spied-predicates* nil))



;;;***********************************************************

;;;NODE
(defclass node ()
  ((body :accessor node-body :initarg :body)))

;;;ORDINARY-NODE
(defclass ordinary-node (node)())

;;;ORDINARY-NODE? ---
(defun ordinary-node? (node)
  (typep node 'ordinary-node))

(defun make-ordinary-node (body)
  (make-instance 'ordinary-node :body body))

;;;NOT-NODE
(defclass not-node (node)())

(defun not-node? (node)
  (typep node 'not-node))

(defun make-not-node (body)
  (make-instance 'not-node :body body))

;;;CUT-NODE
(defclass cut-node (node)
  ((body :initform 'cut)))

(defun make-cut-node ()
  (make-instance 'cut-node))


(defun cut-node? (node)
  (typep node 'cut-node))

;;;AND-NODE
(defclass and-node (node)())

(defun make-and-node (body)
  (make-instance 'and-node :body body))

(defun and-node? (node)
  (typep node 'and-node))

;;;OR-NODE
(defclass or-node (node)())

(defun make-or-node (body)
  (make-instance 'or-node :body body))

(defun or-node? (node)
  (typep node 'or-node))


;;;EXISTS-NODE
(defclass exists-node (node)
  ((vars :initarg :vars)))

(defun exists-node? (node)
  (typep node 'exists-node))

(defun make-exists-node (vars body)
  (make-instance 'exists-node
                 :body body
                 :vars (if (listp vars)
                           vars
                           (list vars))))

(defclass forall-node (node)
  ((vars :initarg :vars)))

(defun forall-node? (node)
  (typep node 'forall-node))

(defun make-forall-node (vars body)
  (make-instance 'forall-node
                 :body (check-forall-node vars body)
                 :vars (if (listp vars)
                           vars
                           (list vars))))

(defun check-forall-node (vars body)
  (if (and (listp body)
           (= (length body) 3)
           (equal (car body) '=>)
           (listp (second body))
           (listp (third body)))
    body
    (error "~s is not a legal forall pattern"
           (list 'forall vars body))))

(defun transform-forall-goal (node)
  (with-slots (vars body) node
    `(not (exists ,vars
                  (and ,(second body)
                       ,(negate-goal (third body)))))))

(defun negate-goal (goal)
  (if (equal (first goal) 'not)
    (second goal)
    (list 'not goal)))
    

;;;HOLDS-NODE
(defclass holds-node (node)
  ((rel  :accessor holds-node-rel :initarg :rel)
   (args :accessor holds-node-args :initarg :args)))


(defclass goal-holds-node (node)())

(defun holds-node? (node)
  (typep node 'holds-node))


(defun goal-holds-node? (node)
  (typep node 'goal-holds-node))

(defun make-holds-node ( rel args )
  (make-instance 'holds-node 
                 :rel rel
                 :args args
                 :body (cons rel args)))         

(defun make-goal-holds-node ( body)
  (make-instance 'goal-holds-node 
    :body body))
                 


(defun make-node (goal env)
  (declare (ignore env))
  (cond ((eq goal 'cut)
	 (make-cut-node))
        (t
	 (case (car goal);;;;(instantiate (car goal) env)
	   (not (make-not-node (second goal)))
           (exists (make-exists-node (second goal)(third goal)))
           (forall (make-forall-node (second goal)(third goal)))
           (asserted (make-ordinary-node goal))
           (holds (make-holds-node  (second goal)
                                    (cddr goal)))
           (goal-holds (make-goal-holds-node (second goal)))
           (and (make-and-node (cdr goal)))
           (or (make-or-node goal))
           (otherwise (make-ordinary-node goal))))))

                     

;;;***************************************************************



;;;To invoke the backward chainer just call the function prove with argument the expression
;;;to be proved. prove returns multiple values <wm-patterns> <bindings>, where <wm-patterns>
;;;are the working memory patterns that match the expression and <bindings> are the resulting
;;;environments.
;;;The algorithm works by creating and running an and/or tree whose root is the state associated
;;;with the top level expression to be proved.

;;;A state has the following structure:
(defstruct 
  (state
   (:conc-name nil)
   (:print-function 
    (lambda (state stream print-depth)
      print-depth
      (format stream "#<STATE ~S>"
              (if (current-goal state)
                (cond
                 ((holds-node? (current-goal state))
                  `(HOLDS 
                    ,(holds-node-rel (current-goal state))
                    ,@(holds-node-args (current-goal state))))
                 ((goal-holds-node? (current-goal state))
                  `(GOAL-HOLDS 
                    ,(node-body (current-goal state))))
                 ((not-node? (current-goal state))
                  `(NOT ,(node-body (current-goal state))))
                 (t
                  (node-body (current-goal state))))
                :ROOT-STATE)))))
  (check-cycles?)
  (current-goal)
  (candidate-lisp-fun)
  (candidate-instances)
  (subclass-state)
  (candidate-classes)
  (candidate-clauses)
  ;;;  (candidates)
  (env)                       ;;The current environment 
  (depth)                     ;;The depth of the node in the and/or tree. 
  (backtrack-point)	      ;;The node that precedes this at the same level in the tree
  (continuation)	      ;;The node that follows this at the same level in the tree
  (parent-node)		      ;;The node above this one
  (children)		      ;;The node below this one
  (proved?)		      ;;Tells whether this state has succeeded
  (returned-env)	      ;;The environment returned by a successful query
  (other-envs)		      ;;This is used to record additional hits
  (redo-point))



(defun proved-state? (thing)  
  (and (state-p thing)
        (proved? thing)))

(defun successors (state)
  (let ((continuation (continuation state)))
    (and continuation
          (cons continuation (successors continuation)))))

(defun candidates (state)
  (or (candidate-lisp-fun state)
      (candidate-instances state)
      (candidate-classes state)
      (candidate-clauses state)))

;(defun *prove (goals &key all query env &aux (root (create-root)))
;  (make-child-state root goals env)
;  (loop
;   with bindings
;   for proved-state = (prove-root root)
;   then (when (or all query)
;	  (catch 'cut
;            (prove-root root  proved-state)))   
;   while proved-state
;   for binding = (returned-env proved-state)
;   do
;   (cond (query 
;          (format t "~2%Solution: ~A  " (instantiate goals binding))
;          (unless (or all (y-or-n-p "~2%More solutions? "))
;            (return :quit)))
;         (t
;          (push binding bindings)))
;   finally
;   (if query                               ;;If we are in query mode, we just print something
;       (unless *inside-or-query*
;         (format t "~2%No more solutions"))   ;;appropriate, otherwise we return 
;       (return 
;        (values bindings  #'(lambda () (prove-root root proved-state)))))))
;
;

;;;*PROVE ---
;;;This function returns one of
;;; :FAIL (if the proof fails)
;;;  list of envs (if all is t and proof succeeds)
;;;  env (if all is nil and proof succeeds)
;;;  :QUIT (if query is t and the user says no more please)
;;;  nil (if query is t and the user does not quit)
(defun *prove (goals &key all query env &aux (root (create-root)))
  ;;;;(setf goals (filter-holds-kappa-goals goals))
  (make-child-state root goals env)
  (multiple-value-bind (proved-state binding)
                       (prove-root root)
    (if proved-state
      (cond (query 
             (format t "~2%Solution: ~s  " (instantiate goals binding))
             (if (or all (y-or-n-p "~2%More solutions? "))
               (*prove-aux goals root proved-state nil query all)
               :quit))
          (all
           (*prove-aux goals root proved-state (list binding) query all))
          (t
           (values binding 
                   #'(lambda () 
                       (*prove-aux goals root proved-state )))))
      (if (and query (not *inside-or-query*))
        (format t "~2%No more solutions")
        :fail))))


(defun filter-holds-kappa-goals (goals )
  (mapcar #'(lambda (goal)
              (if (eq (car goal)'holds)
                (destructuring-bind (holds rel &rest args)
                                    goal
                  holds ;;;ignore
                  (if (and (listp rel)
                           (eq (car rel)'kappa))
                    (multiple-value-bind (schema body)
                                         (rename-args-and-body (second rel)
                                                               (third rel))
                      `(holds (kappa ,schema ,body),@args))
                    `(holds ,rel ,@args)))
                goal))
          goals))
                

(defun *prove-aux (goals root proved-state &optional bindings  query all)   
  (loop 
    with binding
    with flag = t
    do
    (multiple-value-setq (proved-state binding)
        (catch 'cut
          (prove-root root  proved-state)))
    (setf flag (and proved-state (or all query)))
    (when proved-state
        (cond (query 
               (format t "~2%Solution: ~s  " (instantiate goals binding))
               (unless (or all (y-or-n-p "~2%More solutions? "))
                 (return :quit)))
              (all
               (push binding bindings))))
    while flag
    finally
      (cond (query                               ;;If we are in query mode, we just print something
             (unless *inside-or-query*
               (format t "~2%No more solutions")))   ;;appropriate, otherwise we return 
            
            (all
             (return (or bindings :fail)))
            ((not proved-state)
             (return :fail))
            (t
             (return 
              (values binding  #'(lambda () 
                                   (*prove-aux goals root proved-state))))))))



(defun create-root ()
  (make-state :depth -1))

(defmethod root? (s)
  (= (depth s) -1))

(defun find-root-from-state (s)
  (let ((parent-node (parent-node s)))
    (if (root? parent-node)
        s
        (find-root-from-state  parent-node))))

(defun prove-root (s &optional redo-state)
  (catch 'cut
    (let ((state 
           (prove-a-state (redo-point s)redo-state)))
      (when state
        (values state (returned-env state))))))


;;;-----------------------

(defmethod set-candidates ((node node)state)
  (with-slots (body) node
    (let ((rel (car body))
          (env (env state))
          thing)         
      (when (variable? rel)
        (setf rel (ocml-eval-var-or-error rel env)))
      (setf thing (get-relation rel)) 
      (cond (thing
             (multiple-value-bind (lisp-fun instances  classes clauses)
                                  (generate-candidates thing  rel (cdr body) env
                                                       (subclass-state state))
	       (setf  (candidate-lisp-fun state) lisp-fun
		      (candidate-instances state)instances
		      (candidate-classes state)classes
		      (candidate-clauses  state)clauses
                      (check-cycles? state) (avoid-infinite-loop thing))))
            ((member rel '(and exists forall holds goal-holds))
             nil)

            ;;new clause 13/2/01
            ((eq rel 'or)
             (setf  (candidate-lisp-fun state) nil
                    (candidate-instances state)nil
                    (candidate-clauses  state)(make-or-node-clauses (node-body node))))

            ((eq rel 'asserted)
             (setf thing (get-relation (car (second body))))
             (if thing
               (multiple-value-bind (lisp-fun instances classes clauses)
                                    (generate-candidates thing  (name thing) (cdr (second body)) 
                                                         env 
                                                         (subclass-state state))
                 (declare (ignore lisp-fun clauses))
	         (setf  (candidate-lisp-fun state) nil
		        (candidate-instances state)instances
		        (candidate-classes state)classes
		        (candidate-clauses  state)nil))
	       (undefined-relation rel body)))
	    (t (undefined-relation rel body))))))

(defun undefined-relation (rel exp)
  (format *error-output* "undefined-relation: ~S ~S~%" rel exp)
  (ecase *ignore-undefined-relations*
    ((:error) (error "Relation ~s undefined in goal ~s." rel exp))
    ((:ignore) nil)
    ((:warn) (ocml-warn "Relation ~s undefined in goal ~s." rel exp))))

(defmethod set-candidates ((node cut-node)state)
  state ;;;ignore 
  nil)

(defmethod set-candidates ((node and-node)state)
  state ;;;ignore 
  nil)

(defmethod set-candidates ((node or-node) state)
  (setf  (candidate-lisp-fun state) nil
         (candidate-instances state)nil
        ;;;; (candidate-classes state)nil
         (candidate-clauses  state)(make-or-node-clauses (node-body node))))

(defun make-or-node-clauses (consequent)
  ;;;;(setf consequent (rename-variables consequent))
    (mapcar #'(lambda (clause)
                (make-bc-rule-clause (list consequent
                                           'if
                                           clause)))
            (cdr consequent)))

       

(defmethod set-candidates ((node exists-node)state)
  state ;;;ignore 
  nil)

(defmethod set-candidates ((node forall-node)state)
  state ;;;ignore 
  nil)

(defmethod set-candidates ((node holds-node)state)
  state ;;;ignore 
  nil)

(defmethod set-candidates ((node goal-holds-node)state)
  state ;;;ignore 
  nil)

(defun redo-state (state redo?)
  (catch 'cut
    (prove-a-state state redo?)))

;;;PROVE-A-STATE ---Attempts to prove a node in the and-or proof tree.  
(defun prove-a-state (self &optional redo? calling-not-body)
  (let ((current-goal (current-goal self))
        (env (env self))
        (depth (depth self))
        
        (continuation (continuation self))
        (redo-point (redo-point self))
        (redo-flag (and (state-p redo?)
                        (not (eq self redo?))))
        result)
    
    (trace-goal current-goal env depth redo?)
    (setf result
        (cond
         (redo-flag 
	  (redo-state redo-point redo?))
	 ((or (ordinary-node? current-goal)
              (or-node? current-goal))
	  (prove-goal self (node-body current-goal) redo?))
	 ((not-node? current-goal)
          (unless redo?
	    (prove-not-node self )))     
	 ((cut-node? current-goal)        
	  (if redo?                       
	      (perform-cut self)
	      (prove-cut-node self)))
         ((and-node? current-goal)
          (if redo?
              (unless (state-p redo?)
		(redo-state redo-point redo?))
	      (create-dummy-child-state self (node-body current-goal) env)))
         ((exists-node? current-goal)
          (if redo?
              (unless (state-p redo?)
                (redo-state redo-point redo?))
	      (create-dummy-child-state self (list (node-body current-goal)) env)))
         ((forall-node? current-goal)
          (if redo?
              (unless (state-p redo?)
                (redo-state redo-point redo?))
	      (create-dummy-child-state self (list (transform-forall-goal current-goal)) env)))
         ((goal-holds-node? current-goal)
          (if redo?
            (unless (state-p redo?)  ;;;If redo? is a state => redo-flag = nil
              (redo-state redo-point redo?))
            (create-dummy-child-state self
                                      (List (eval-functional-arg-or-instantiate-rel
                                             (node-body current-goal)env))
                                        env)))
         ((holds-node? current-goal)
          (if redo?
            (unless (state-p redo?)  ;;;If redo? is a state => redo-flag = nil
              ;;;which means the child of this state
              ;;;has failed.  In this case we fail,
              ;;;because a holds state can only be proved
              ;;;by proving its child (which in this case
              ;;;we know has failed).
              (redo-state redo-point redo?))
            (let ((rel (eval-functional-arg-or-instantiate-rel
                          (holds-node-rel current-goal)
                          env)))
              (cond ((and (listp rel)
                          (eq (car rel) 'kappa))
                     (multiple-value-bind (schema body)
                                          (rename-args-body-and-free-vars 
                                           (second rel)
                                           (third rel)
                                           env)
                       (create-dummy-child-state self 
                                                 (list body)
                                               (multiple-variable-bind 
                                                schema 
                                                (new-evaluate-args (holds-node-args current-goal)
                                                                   env
                                                                   #'eval-functional-arg)
                                                env))))
                    (t
                     (create-dummy-child-state self
					       (List (cons rel (holds-node-args current-goal)))
                                               env))))))
					
                                        
	 (t
	  (error "Internal error. Can't understand goal ~A" current-goal))))
    (cond ((proved-state? result)
            ;;;; (unless (eq redo? :backtrack)
	     (trace-success current-goal (returned-env result) depth)
	     (if (and continuation (not calling-not-body))
		 (prove-continuation self result continuation)
                 result))
            (t
	     (trace-failure current-goal env depth)
             (unless calling-not-body ;;;;;(eq redo? :backtrack))
               (if redo-flag 
                   (prove-a-state self self)
		   (backtrack-from self)))))))
             
      
(defun prove-not-node (self &aux body result 
                            original-goal 
                            continuation-temp)
  (let ((current-goal (current-goal self))
        (env (env self))
        (continuation (continuation self)))
    (setf body (node-body current-goal))
    (unwind-protect             
      ;;This is to be on the safe side. 
	(progn
          ;;Changing the relevant slots to ensure body of NOT
          ;;is treated as a 'standalone' goal
	  (setf (depth self) (1+ (depth self))	
		continuation-temp continuation 
		(continuation self) nil
		original-goal current-goal  
		(current-goal self) (make-node
			             body env)                                
		result (prove-a-state self nil t))
	  (unless (proved-state? result)  
	    (make-this-state-proved
	     self env)))
      ;;Let's restore original goal and depth
      (setf (current-goal self) original-goal             
	    (continuation self) continuation-temp
            (depth self) (1- (depth self))))))

;;;PROVE-GOAL
;;;Attempts to prove a pattern such as (rel {term1.......termn}), associated with a state, say s.  
(defun prove-goal (state pattern &optional redo?);;; &aux rel args)
  (let ((env (env state))
        (other-envs (other-envs state))
        redo-point)
    (cond ((and redo? other-envs)                          ;;;We have been here before and we have 
           (make-this-state-proved state		   ;;;previously generated solutions
                                   (car other-envs)
                                   (cdr  other-envs)))
          ((and (eq redo? :backtrack)
                (setf redo-point (redo-point state)))
           (catch 'cut 
	     (let ((result (prove-a-state redo-point redo?)))
	       (or result
		   (when (candidates state)
                     (trace-goal (current-goal state)  env (depth state) redo?)
                     (try-candidate state
                                    (cons (car pattern)
                                          (new-evaluate-args (cdr pattern)  env
                                                             #'eval-functional-arg))))))))
	  ((candidates state)
           (unless (endless-loop? state) ;;;addded 25/10/01
             (try-candidate state (cons (car pattern)
                                        (new-evaluate-args (cdr pattern)  env
							   #'eval-functional-arg))))))))

(defun endless-loop? (state)
  (if (and (check-cycles? state)
           (parent-node state))
    (check-parent-state (instantiate (node-body (current-goal state)) (env state))
                        (parent-node state))))

(defun check-parent-state (goal parent-state)
  (if (same-state? goal parent-state)
    t
    (when (parent-node parent-state)
      (check-parent-state goal (parent-node parent-state)))))

(defun same-state? (goal state)
  ;;(setf a goal b state)
 ;; (break)
  (unless (root? state)
    (let ((goal2 (instantiate (node-body(current-goal state)) (env state))))
      (and (eq (car goal2)
               (car goal))
           (not (eq (unify (cdr goal)(cdr goal2))
                    :fail))))))



;;;CACHE-RESULT ---Caches the result of a query in the cache-table of the associated relation
(defun cache-result (rel pargs result index-table &aux vars envs)
  (cond ((proved-state? result)
         (setf vars (mapcar #'car index-table)
               envs (mapcar #'(lambda (env)
                                (relevant-env nil env vars))
                            (cons (returned-env result)
			          (other-envs result))))
         (cache-value-internal
          rel
          pargs
          (loop for var in vars
		do
	        (setf envs (subst (right-value var index-table)var envs))
                finally
                (return envs))))
        (t
         (cache-value-internal rel pargs :fail)))
  result)


(defun process-args (args env)
  (let ((*index* 0))
    (declare (special *index* *index-table*))
    (values 
     (mapcar #'(lambda (arg)(process-this-arg arg env))
             args)
     *index-table*)))

(defun process-this-arg (arg env)
  (declare (special  *index-table*))
  (cond ((atom arg)
         (if (variable? arg)
             (multiple-value-bind (flag result)
                 (lookup arg env)
               (cond (flag
		      (cond ((ground-termp result)
                             result)
                            ((atom result)
                             (or (right-value arg *index-table*)
                                 (right-value result *index-table*)
                                 (Let ((placeholder (generate-var-placeholder)))
			           (push (cons arg placeholder) *index-table*)
                                   placeholder)))
			    (t
                             (mapcar #'(lambda (x)(process-this-arg x env))result))))
                     (t
                      (or (right-value arg *index-table*)
			  (Let ((placeholder (generate-var-placeholder)))
			    (push (cons arg placeholder) *index-table*)
			    placeholder)))))
             arg))
        (t
         (cons (process-this-arg (car arg) env)
               (process-this-arg (cdr arg) env)))))

(defun generate-var-placeholder ()
  (declare (special  *index*))
  (prog1
      (read-from-string (string-append "?%%%"(prin1-to-string *index*)))
    (incf *index*)))
                                                                
;;;TRY-CANDIDATE --
(defun  try-candidate (self pattern  &aux  candidates)
  (cond ((or-node? (current-goal self))
         (try-candidate-clauses self pattern (candidate-clauses self)))
        ((setf candidates (candidate-lisp-fun self))
         (setf (candidate-lisp-fun self) nil)
         (let ((result (match-candidate candidates self pattern)))
           (if (Proved-state? result)
               result
               (try-candidate-instances self pattern (candidate-instances self)))))
        (t
         (try-candidate-instances self pattern (candidate-instances self)))))

(defun try-candidate-instances (state pattern candidate-instances)
  (let ((result (try-candidate-instances2 state pattern candidate-instances)))
    (if (proved-state? result)
        result
        (let* ((asserted? (eq (car pattern)'asserted)))
              ;; (slot-of (unless asserted?
               ;;           (slot-of (get-relation (car pattern))))))
          (unless asserted?
            (try-candidate-clauses state pattern (candidate-clauses state)))))))

          ;;(if (or slot-of asserted?)
          ;;    (try-candidate-classes state pattern (candidate-classes state)slot-of asserted?)
	   ;;   (try-candidate-clauses state pattern (candidate-clauses state)))))))

(defun try-candidate-instances2 (state pattern candidate-instances)
  (loop for c in candidate-instances
        for result = (match-candidate c state pattern)
        do
        (setf (candidate-instances state)
              (cdr (candidate-instances state)))
        until (proved-state? result)
        finally
        (return result)))


;(defun try-candidate-clauses (state pattern candidate-clauses);;; &optional slot-of)
;  (loop with result
;        for c in candidate-clauses
;	do
;        (setf (candidate-clauses state)
;              (cdr (candidate-clauses state)))
;        (setf result (match-candidate c state pattern))
;        until (proved-state? result)
;        finally
;        (return 
;         (if (proved-state? result)
;             result
;             (try-candidate-classes state pattern (candidate-classes state))))))

;;;redefined 25/10/01 to handle iff-def correctly
(defun try-candidate-clauses (state pattern candidate-clauses);;; &optional slot-of)
  (loop with result
        with exit-flag
        for c in candidate-clauses
	do
        (setf (candidate-clauses state)
              (cdr (candidate-clauses state)))
        (setf result (match-candidate c state pattern))
        until (or (proved-state? result)
                  (setf exit-flag (iff-def-clause? c)))
        finally
        (return
         (cond ((proved-state? result)
                result)
               (exit-flag ;;if we have encountered an iff-def and failed, 
                          ;;then we do not try alternatives
                (setf (candidate-clauses state) nil))
               (t
                (try-candidate-classes state pattern (candidate-classes state)))))))




             
            

(defun try-candidate-classes (state pattern candidate-classes);;; &optional slot-of asserted?)
  ;;if slot-of is nil, then we are trying to prove a goal such as (class ?X)
  ;;otherwise a goal such as (slot ?x ?c)
  (loop with result
        for c in candidate-classes
	do
        (setf result (try-candidate-class c state pattern)) ;;;; slot-of asserted?))
        (setf (candidate-classes state)
              (cdr (candidate-classes state)))
	until (proved-state? result)
        finally
        (return
         (if (proved-state? result)
            result))))
            ;;;(when (and slot-of
            ;;;           (not asserted?))
	    ;;;  (try-candidate-clauses state pattern (candidate-clauses state)slot-of))))))


(defun try-candidate-class (class state pattern);; slot-of asserted?)
  ;  (if (or slot-of asserted?)
  ;      (try-candidate-instances2 state
  ;                                pattern
  ;                                (setf (candidate-instances state)
  ;                                      (get-current-direct-instances class)))
  (catch 'cut
    (prove-a-state
     (make-child-state state
                       (list (cons (name class)(cdr pattern)))
                       (env state)
                       'subclass-state))))
 

                   

;;;MATCH-CANDIDATE RELATION-INSTANCE
(defmethod match-candidate ((candidate relation-instance) state goal)
  (when (eq (car goal) 'asserted)
    (setf goal (second goal)))
  (with-slots (args) candidate
      (let ((result (match args (cdr goal) (env state))))
        (unless (eq result :fail)
          (make-this-state-proved state result)))))

;;; MATCH-CANDIDATE BASIC-DOMAIN-CLASS
(defmethod match-candidate ((candidate basic-domain-class) state goal
                            &aux instance-var spec)
  (when (eq (car goal) 'asserted)
    (setf goal (second goal)))
  (setf instance-var (second goal))
    (if (= (length goal) 3)             ;;the goal is something like (<slot> <instance> <value)
	(setf spec (list (car goal)(third goal)))
	(setf spec (cddr goal)))
    (Let ((result (match-spec-against-instance candidate instance-var spec (env state))))
      (unless (eq result :fail)
	(make-this-state-proved state (car result)(cdr result)))))

;;;MATCH-CANDIDATE  BC-CLAUSE
(defmethod match-candidate ((bc-clause bc-clause) state goal)
  (rename-bc-clause-variables bc-clause)
  (Let ((result (match-goal-against-clause bc-clause goal (env state))))
    (unless (eq result :fail)
      (let ((new-goals (cdr (renamed-instance bc-clause))))
        (if new-goals
            (catch 'cut
	      (prove-a-state
	       (make-child-state state  new-goals result)))
	    (make-this-state-proved state result ))))))


;;;MATCH-CANDIDATE LISP-FUNCTION
;;;NOTE: The goal is not necessarily a ground pattern.  It is the responsibility of
;;;each lisp function to ensure they handle logical vars correctly (e.g. by calling instantiate).
(defmethod match-candidate (lisp-function state goal )
    (let ((envs (apply lisp-function (append (cdr goal)(List (env state))))))
      (unless (eq envs :fail)      ;;;Now behaves like the other cases
        (make-this-state-proved
         state (car envs) (cdr envs)))))

;;;MATCH-GOAL-AGAINST-CLAUSE
(defun  match-goal-against-clause (bc-clause goal env)
  (with-slots (renamed-instance) bc-clause
    (unify (car renamed-instance) goal env)))


(defmethod rename-bc-clause-variables ((bc-clause bc-clause))
  (with-slots (antecedents consequent ;;;; variables
                           renamed-instance) bc-clause
    (setf renamed-instance
          (rename-variables (cons consequent antecedents)))))


;;;PROVE-CUT-NODE
(defun prove-cut-node (state)
  (make-this-state-proved state))

(defun make-this-state-proved (self
                                   &optional (environment (env self))
				   other-environments)
  (setf (returned-env self) environment
        (proved? self) t
        (other-envs self) other-environments)
  self)


;;;;Just a reminder. When you perform a cut, both all your predecessors and your parent node fail.
;;;Given our representation of the and/or tree we only need to fail the parent node
(defun perform-cut (self)
  (setf (candidate-clauses (parent-node self))nil)
  (throw 'cut nil))
  

;;;CREATE-DUMMY-CHILD-STATE --This is defined to handle ` AND goals'
(defmethod create-dummy-child-state (state goals env)
  (if goals
    (catch 'cut
      (prove-a-state
       (make-child-state state  goals env)))
    (make-this-state-proved state env )))

(defun instantiate-backward-rule (bc-clause  env)
  (instantiate
   (cdr (renamed-instance bc-clause)) ;;;this retrieves the antecedents
   env))



(defun make-child-state (self new-goals environment &optional subclass-state) ;;; &optional ignore-instances)
  
  (let* ((goal (if (consp (first new-goals))
                 (maybe-canonicalise-frame-expression (first new-goals))
                 (first new-goals)) )
          (current-goal (make-node goal environment))
          (son
           (make-state
            :subclass-state subclass-state
	    :env environment
	    :depth (1+ (depth self))
	    :parent-node self
	    :current-goal current-goal
	    :continuation (cdr new-goals))))
    (set-candidates current-goal son )
    (push son (children self))
    (Setf (redo-point self) son)
    son))

(defun maybe-canonicalise-frame-expression (exp)
  (if (eq exp 'cut)
    exp
  (let ((rel (car exp)))
    (if (and (get-domain-class rel)
               (cddr exp))
      `(and ,(list rel (second exp))
            ,@(loop for i from 2 upto (- (length exp)2) by 2
                    collect (list (elt exp i)(second exp)(elt exp (1+ i)))))
        exp))))


;;;PROVE-CONTINUATION
;;;A continuation of a state s is the state that follows s at the same level of the tree.
;;;For instance, in the prolog clause A(x) if B(x), C(x) the state associated with C is
;;;the continuation of the one associated with B.
;;; This function is called when state has been proved by proving his child, proved-state.
;;;continuation is the continuation of state.
(defun prove-continuation (self proved-state new-continuation)
  (cond ((state-p new-continuation)
	   (setf (env new-continuation)
	         (returned-env proved-state)       ;;We pass it the current environment
                 (redo-point new-continuation) ;;this to get rid of the redo point from
                 nil                           ;;the last time we were here
                ;;; (backtrack-point new-continuation)
                ;;; proved-state
                 )
	         ;;;;(candidates new-continuation)		;;Not very nice code!
	   (set-candidates (current-goal
			     new-continuation)
			    new-continuation)
	   (prove-a-state new-continuation))
          (t    ;;;continuation is a list of clauses. Therefore, we transform it into a state. 
                (let ((continuation2 (make-continuation self proved-state new-continuation)))
                  (setf (continuation self)  continuation2)
                  (prove-a-state continuation2)))))

(defun backtrack-from (state)
  (let ((backtrack-point (backtrack-point state)))
    (when backtrack-point
      (prove-a-state backtrack-point :backtrack))))


(defun make-continuation (self proved-state continuation1)
  (let* ((goal  (if (consp (first continuation1))
                    (maybe-canonicalise-frame-expression (first continuation1))
                    (first continuation1)))
           (parent-node(parent-node self))
           (env (returned-env proved-state))
           (goal-node (make-node goal env))
           (son (make-state
		;;;;; :candidates (find-candidates goal-node)
	         :current-goal goal-node
	         :env env
	         :depth (depth self)
	         :backtrack-point (if (not-node?  (current-goal self))
                                      (backtrack-point self)
                                      self
                                     ;;;; proved-state ;;;;self --changed 19/10/95--enrico
				      )
	         :parent-node parent-node
	         :continuation (cdr continuation1))))
      (set-candidates goal-node son)
      (when parent-node
        (Setf (redo-point parent-node) son)
        (push son (children parent-node)))
      son))

 

;(defmethod print-object ((state state) stream)
;  (with-slots (current-goal env)state
;    (format t "#<STATE ~S>" (node-body current-goal) )))


;;;***********************************************************
(defun relevant-env (pattern env &optional (vars (collect-variables pattern)))
  (mapcan #'(lambda (var)
                (multiple-value-bind (flag val)
                    (lookup var env)
                  (when flag
                    (list (cons var val)))))

		  ;  (if (variable? val)
		;	(when (member val vars)
		;	  (list (cons var val)))
			;(list (cons var val))))))
          vars))

;(defun relevant-env (pattern env)
;  (mapcan #'(lambda (var)
;                (multiple-value-bind (flag val)
;                    (lookup var env)
;                  (when flag
;                    (list (cons var val)))))
;            (collect-variables pattern)))

                

(defmethod trace-goal ((goal node) env  depth &optional redo?)
  (with-slots (body) goal
      (when (spying? (car body))
      (print-with-spaces (+ depth *depth*)
	                 (if redo?
		             "~s- Redo ~s in ~s"
		             "~s- Goal: ~s, env: ~s")
	                 depth body (relevant-env body env)))))

(defmethod trace-goal ((goal holds-node) env  depth &optional redo?)
  (with-slots (body) goal
      (when (spying? (car body))
      (print-with-spaces (+ depth *depth*)
	                 (if redo?
		             "~s- Redo (HOLDS ~s) in ~s"
		             "~s- Goal: (HOLDS ~s), env: ~s")
	                 depth body (relevant-env body env)))))

(defmethod trace-goal ((goal goal-holds-node) env  depth &optional redo?)
  (with-slots (body) goal
    (when (variable? body)
      (setf body (lookup-or-self body env)))
      (when (and (consp body)
                 (spying? (car body)))
      (print-with-spaces (+ depth *depth*)
	                 (if redo?
		             "~s- Redo (GOAL-HOLDS ~s) in ~s"
		             "~s- Goal: (GOAL-HOLDS ~s), env: ~s")
	                 depth body (relevant-env body env)))))



(defmethod trace-goal ((goal not-node) env  depth 
                       &optional redo?)
  (with-slots (body) goal
    (when (spying? 'not)
      (print-with-spaces 
       (+ depth *depth*)
       (if redo?
         "~s- Redo (NOT ~s) in ~s"
         "~s- Goal: (NOT ~s), env: ~s")
       depth body  (relevant-env body env)))))

(defmethod trace-goal ((goal and-node) env  depth &optional redo?)
  (with-slots (body) goal
      (when (spying? 'and)
      (print-with-spaces (+ depth *depth*)
	                 (if redo?
		             "~s- Redo (AND ~{~s ~}) in ~s"
		             "~s- Goal: (AND ~{~s ~}), env: ~s")
	                 depth body  (relevant-env body env)))))

(defmethod trace-goal ((goal or-node) env  depth &optional redo?)
  (with-slots (body) goal
      (when (spying? 'or)
      (print-with-spaces (+ depth *depth*)
	                 (if redo?
		             "~s- Redo ~s in ~s"
		             "~s- Goal: ~s, env: ~s")
	                 depth body  (relevant-env body env)))))

(defmethod trace-goal ((goal exists-node) env  depth &optional redo?)
  (with-slots (body vars) goal
      (when (spying? 'exists)   
      (print-with-spaces (+ depth *depth*)
	                 (if redo?
		             "~s- Redo (EXISTS ~{~s ~} ~s) in ~s"
		             "~s- Goal: (EXISTS ~{~s ~} ~s), env: ~s")
	                 depth vars body  (relevant-env body env)))))

(defmethod trace-goal ((goal forall-node) env  depth &optional redo?)
  (with-slots (body vars) goal
      (when (spying? 'forall)   
      (print-with-spaces (+ depth *depth*)
	                 (if redo?
		             "~s- Redo (FORALL ~{~s ~} ~s) in ~s"
		             "~s- Goal: (FORALL ~{~s ~} ~s), env: ~s")
	                 depth vars body  (relevant-env body env)))))

(defmethod trace-goal ((goal cut-node) env  depth &optional redo?)
                            
  (with-slots (body) goal
      (when (spying? 'cut)
      (print-with-spaces (+ depth *depth*)
	                 (if redo?
		             "~s- Perform CUT"
		             "~s- Goal: ~s, env: ~s")
	                 depth body  (relevant-env body env)))))

(defmethod trace-goal ((goal (eql nil)) env  depth &optional redo?)
  env depth redo?)


(defmethod  trace-failure ((goal (eql nil)) env  depth)
 env depth )

(defmethod  trace-success ((goal (eql nil)) env  depth)
  env depth )

(defmethod  trace-failure ((goal not-node) env  depth)
  (with-slots (body) goal
      (when (spying? 'not)
    (print-with-spaces (+ depth *depth*)
	            "~s- Fail (NOT ~s) in ~s. " depth 
	            body (relevant-env body env)))))

(defmethod  trace-failure ((goal node) env  depth)
  (with-slots (body) goal
      (when (spying? (car body))
    (print-with-spaces (+ depth *depth*)
	            "~s- Fail ~s in ~s. " depth 
	            body  (relevant-env body env)))))

(defmethod  trace-failure ((goal holds-node) env  depth)
  (with-slots (body) goal
      (when (spying? (car body))
    (print-with-spaces (+ depth *depth*)
	            "~s- Fail (HOLDS ~s) in ~s. " depth 
	            body  (relevant-env body env)))))

(defmethod  trace-failure ((goal goal-holds-node) env  depth)
  (with-slots (body) goal
      (when (spying? (car body))
    (print-with-spaces (+ depth *depth*)
	            "~s- Fail (GOAL-HOLDS ~s) in ~s. " depth 
	            body  (relevant-env body env)))))

(defmethod  trace-failure ((goal and-node) env  depth)
  (with-slots (body) goal
      (when (spying? 'and)
    (print-with-spaces (+ depth *depth*)
	            "~s- Fail (AND ~{~s ~}) in ~s. " depth 
	            body  (relevant-env body env)))))

(defmethod  trace-failure ((goal OR-node) env  depth)
  (with-slots (body) goal
      (when (spying? 'or)
    (print-with-spaces (+ depth *depth*)
	            "~s- Fail ~s in ~s. " depth 
	            body  (relevant-env body env)))))

(defmethod  trace-failure ((goal exists-node) env  depth)
  (with-slots (body vars) goal
      (when (spying? 'exists)
    (print-with-spaces (+ depth *depth*)
	            "~s- Fail (EXISTS ~{~s ~} ~s) in ~s. " depth vars
	            body  (relevant-env body env)))))

(defmethod  trace-failure ((goal forall-node) env  depth)
  (with-slots (body vars) goal
      (when (spying? 'forall)
    (print-with-spaces (+ depth *depth*)
	            "~s- Fail (FORALL ~{~s ~} ~s) in ~s. " depth vars
	            body  (relevant-env body env)))))


(defmethod  trace-success ((goal node) env  depth)
  (with-slots (body) goal
      (when (spying? (car body))
    (print-with-spaces  (+ depth *depth*)
	            "~s- Proved ~s in ~s"
	            depth body (relevant-env body env)))))

(defmethod  trace-success ((goal holds-node) env  depth)
  (with-slots (body) goal
      (when (spying? (car body))
    (print-with-spaces  (+ depth *depth*)
	            "~s- Proved (HOLDS ~s) in ~s"
	            depth body (relevant-env body env)))))

(defmethod  trace-success ((goal goal-holds-node) env  depth)
  (with-slots (body) goal
      (when (spying? (car body))
    (print-with-spaces  (+ depth *depth*)
	            "~s- Proved (GOAL-HOLDS ~s) in ~s"
	            depth body (relevant-env body env)))))



(defmethod trace-success ((goal cut-node) env depth)
  (declare (ignore #-:allegro goal env))
  (when (spying? 'cut)
    (print-with-spaces (+ depth *depth*)
		       "~s- Proved CUT" depth )))

(defmethod  trace-success ((goal not-node) env  depth)
  (with-slots (body) goal
      (when (spying? 'not)
    (print-with-spaces (+ depth *depth*)
	            "~s- Proved (NOT ~s) in ~s" depth 
	            body (relevant-env body env)))))

(defmethod  trace-success ((goal and-node) env  depth)
  (with-slots (body) goal
      (when (spying? 'and)
    (print-with-spaces (+ depth *depth*)
	            "~s- Proved (AND ~{~s ~}) in ~s" depth 
	            body  (relevant-env body env)))))

(defmethod  trace-success ((goal or-node) env  depth)
  (with-slots (body) goal
      (when (spying? 'or)
    (print-with-spaces (+ depth *depth*)
	            "~s- Proved ~s in ~s" depth 
	            body  (relevant-env body env)))))

(defmethod  trace-success ((goal exists-node) env  depth)
  (with-slots (body vars) goal
      (when (spying? 'exists)
    (print-with-spaces (+ depth *depth*)
	            "~s- Proved (EXISTS ~{~s ~} ~s) in ~s" depth vars
	            body  (relevant-env body env)))))

(defmethod  trace-success ((goal forall-node) env  depth)
  (with-slots (body vars) goal
      (when (spying? 'forall)
    (print-with-spaces (+ depth *depth*)
	            "~s- Proved (FORALL ~{~s ~} ~s) in ~s" depth vars
	            body  (relevant-env body env)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;Below is a definition of prove-goal, which includes the bit to handle caching of proofs.
;;;This stuff is incomplete (i.e. it does not work), so it has been commented out.  I'll
;;;fix it and put it back one day.
;;;DO NOT DELETE
;(defun prove-goal (state pattern &optional redo? &aux rel args)
;  (let (
;        (env (env state))
;        (other-envs (other-envs state))
;        (redo-point (redo-point state)))
;    (cond
;     ((and redo?  other-envs)
;      (make-this-state-proved state (car other-envs)(cdr  other-envs)))
;     ((and redo? redo-point)           ;;;added to handle backtracking
;      (prove-a-state redo-point :redo))
;     ((candidates state)
;      (setf args (with-default-eval-args-off 
;                                 (evaluate-args  (cdr pattern) env))
;            rel (get-relation (car pattern)))
;      (if (and rel                   ;;(car pattern) may be an operator
;               (cache-values? rel))
;          (Let (*index-table* pargs result)
;            (declare (special *index-table*))
;            (setf pargs (process-args args env)
;		  result (fetch-cached-value rel pargs  *index-table*))
;	    (if result
;		(unless (eq result :fail)
;		  (make-this-state-proved state (car result)(cdr result)))
;		;;there is no cached value
;                (cache-result rel pargs
;                              (try-candidate state
;					     (cons (car pattern)args)
;					     
;                                             )
;                              *index-table*)))
;	  (try-candidate state (cons (car pattern)args)  ))))))