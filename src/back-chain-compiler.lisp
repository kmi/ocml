;;; File: New Support for Backward Chaining in OCML
;;; Author: Mauro Gaspari
;;; Date: August 1995. Revised: June 1996

;;; BC rules are compiled into lisp functions which include also
;;; primitives to deal with OCML relations and conditions.
;;; The implementation is based on success continuations.
;;; In general a function is created for each relations,
;;; except for relations defined with lisp-fun.
;;; and instance relations.
;;; If the second optional parameter of compile-bc-rules is set to nil
;;; a function is defined for each relation istances.
;;; Otherwise relation instances are compiled in-line.

;;; =, true, fail and findall are compiled in line.

;;; Further optimizations are possible:
;;; 1. to compile in line code for get-domain-class and slot-of.
;;; 2. Unification in the head of the clauses must still be optimized 
;;; see make-head-body.

;;; Many other improvements are possible.

;;; The compiler is all in this file and new-ask.lisp is required.
;;; To see the compilation technique used run
;;; (compile-bc-rules t) and inspect the code of
;;; functions with (symbol-funcion)

;;; If you have any problem please ring to gaspari@cs.unibo.it.

(in-package ocml)


;;; Useful continuations.

(defun true-cont (env) (filter-env env))
(defun filter-env (env) (or env t))
(defun fail-cont (env)
  (declare (ignore env)) 
  nil)

;;; This function is used to 

(defun make-parameters (arity &optional (prefix "?") (string "#"))
  "Create parameters of the function associated to a bc rule"
  (loop for i from 1 to arity
        collect (new-intern prefix i string)))


;;;COMPILE-BC-RULE -- generates a function implementing a bc rule.
(defun compile-bc-rule (name rule &optional (trace *compile-trace*))
  "Compile a bc rule generating a lisp function. 
   If the corresponding relation has a sufficient
   it is inserted as first rule.
   If compile is t it also compiles the function."
  (let* (;;;;;(rule (get-rule name))
	 (relation (get-relation (defines-relation rule)))
    	 (arity (arity relation))
	 (fun-name (new-intern name arity))
	 (sufficient (sufficient relation))
         (iff-def (iff-def relation))
	 (clauses (cond ((and iff-def sufficient) 
                         (cons iff-def (cons sufficient (clauses rule))))
                        (iff-def 
                         (cons iff-def (clauses rule)))
                        (sufficient 
                         (cons sufficient (clauses rule)))
                        (t
                         (clauses rule))))
	 (args (make-parameters arity "?" "$"))
	 (cut-tag (when (cut-is-in clauses)
			(gentemp (symbol-name fun-name))))
		  ;; this tag is used for catch and throw when a cut is in the rule
	 (fun 
	  `(defun ,fun-name (,@args env cont)
	     ,@(when (spying? name)
		     `((print-with-spaces *task-level*
					 "~A- Goal: ~A, env: ~A"
					 ',name (list ,@args) env)))
	     .,(add-bindings-and-instances
		(mapcar #'(lambda (clause)
			    (compile-clause args clause 'cont cut-tag 'env))
			clauses)
		cut-tag
		fun-name
		relation
		args))))
    ;;;;;(print (list "TRACE" trace))
    (if trace
      (format t "~%~A" fun)
      (format t "~%~A" fun-name))
    (eval fun)
    (push fun-name *compiled-rules*)
;    (setf (rule-function rule) (symbol-function fun-name))
    fun-name))

(defun cut-is-in (clauses)
  "Check a predicate include a cut in its clauses."
  (when clauses
	(or (member `cut (bc-clause-antecedents (car clauses)))
	    (cut-is-in (cdr clauses)))))
	 

(defun add-bindings-and-instances (compiled-clauses cut-tag fun-name relation args)
  "Undo bindings and add code to deal with instances."
  (cond ((null compiled-clauses)
	 (add-relation-code fun-name (name relation) relation args))
	((eq (length compiled-clauses) 1)
	 (if-cut cut-tag
		 `((or ,@(add-relation-code fun-name (name relation) relation args)
		       ,(car compiled-clauses)))))
	(t
	 (if-cut cut-tag
	    `((or ,@(add-relation-code fun-name (name relation) relation args)
		  ,(car compiled-clauses)
		  ,@(loop for clause in (butlast (cdr compiled-clauses))
			  collect clause)
		  ,@(last (cdr compiled-clauses))))))))

(defun add-relation-code (fun-name name relation args)
  "Add lisp code matching relation instances."
  (let ((slot-of (slot-of relation))
	class)
    	  ;;; it is not necessary to consider lisp-fun because we 
	  ;;; assume that when a relation is defined by a lisp
	  ;;; function it has no instances constraints or rules.
          ;;; The sufficient conditions is included among the other rules.
    (cond 
     (slot-of
      (let ((spec (list name (second args)))
	    (instance-var (first args)))
	(loop for class in slot-of
	      collect
	      (make-slot-of-code class instance-var spec fun-name)
	      into result
	      finally 
	      (return 
	       (nconc result
		      (make-relation-instances-code relation args fun-name)
		      )))))
     ((setf class (get-domain-class name))
      (let ((spec (cdr args))
	    (instance-var (first args)))
	(cons (make-domain-class-code class instance-var spec fun-name)
	      (make-relation-instances-code relation args fun-name))))
     (t (make-relation-instances-code relation args fun-name)))))

(defun if-cut (cut-tag compiled-expr)
  "Insert a catch implementing cut if needed."
  (if cut-tag 
       `((catch ',cut-tag ,@compiled-expr))
    compiled-expr))

(defun compile-clause (args clause cont cut-tag env)
  "Generate unifications for the head of a clause and compile the body."
  (multiple-value-bind (temp-head bindings)
	(compile-head args (cdr (bc-clause-consequent clause)))
    (let* ((temp-body (bc-clause-antecedents clause))
	   (fun-body (make-head-body temp-head temp-body bindings env cont cut-tag))
	   (unbound-vars (set-difference (collect-variables fun-body) args)))
      (if unbound-vars
	  `(let ,(mapcar #'(lambda (var) `(,var (gensym "?")))
			 unbound-vars)
	     ,(car fun-body))
	(car fun-body)))))

;;; Compile the head of a clause

(defun compile-head (args clause-head)
  "Compile the head of a clause.  It returns a list of
   unifications and a list of bindings for the arguments
   of the function implementing the rule."
  (declare (special bindings))
  (let* (bindings
	 (temp-head 
	  (mapcan #'(lambda (x y)
		      (if (variable? y)
			  (let ((item  (assoc y bindings)))
			    (if item 
				(list (list x (cdr item)))
			      (progn 
				(push (cons y x) bindings)
				nil)))
			(list (list x y))))
		  args clause-head)))
    (values temp-head bindings)))

(defun compile-back-term (term bindings env)
  "It compiles a term substituting bindings to variables and
   introducing cons for lists."
  (cond ((variable? term) 
	 (let ((value (cdr (assoc term bindings))))
	   (or value
	       term)))
	((or (null term) (numberp term)) term)
	((atom term) `(quote ,term))
	((consp term) 
	 (let ((first-arg (car term)))
	   (if (ocml-function? first-arg)
;	       (make-ocml-eval-form (cadr term) bindings env)
               (compile-term term bindings env)
	     (list 'cons 
		   (compile-back-term first-arg bindings env)
		   (compile-back-term (cdr term) bindings env)))))))

(defun compile-ocml-args (term bindings)
  "It compiles a term substituting bindings to variables and
   introducing cons for lists."
  (declare (special cenv))
  (cond ((variable? term)
	 (let ((value (cdr (assoc term bindings))))
	   (cond (value term)
		 (t (push (cons term nil) cenv)
		    term))))
	((or (null term) (numberp term)) term)
	((atom term) term)
	((consp term) 
	 (cons (compile-ocml-args (car term) bindings)
	       (compile-ocml-args (cdr term) bindings)))))

(defun make-ocml-eval-form (body bindings env)
  (let (cenv args (nenv (gensym "env")))
    (declare (special cenv))
    (setf args (compile-ocml-args body bindings))
    (if cenv
	`(let ((,nenv ,env))
	   ,@(mapcar #'(lambda (bind)
			 (if (null (cdr bind))
				   `(pushnew (cons ',(car bind) ,(car bind)) ,nenv)
			   `(pushnew ',bind ,nenv)))
		    cenv)
	   (ocml-eval ',args ,nenv))  
      `(ocml-eval ',args ,env))))

;;;; compile the body of a clause
;;;; fail, true and unify are defined as special cases.
    
(defun compile-body (body bindings cont cut-tag env)
  "Compile the body of a clause."
  (if (null body)
      `((funcall ,cont ,env))
    (let ((goal (car body)))
      (cond
       ((and (eq goal 'cut) cut-tag)                           
	`((throw ',cut-tag ,@(compile-body (cdr body) bindings cont nil env))))
       ((and (eq goal 'cut) (null cut-tag))
	(compile-body (rest body) bindings cont nil env)
	(ocml-warn "Only the first cut of the rule is compiled."))
	      ;;; multiple cut in a rule are ignored.
       ((eq goal 'and)
	(compile-body (rest body) bindings cont cut-tag env))
       (t (let ((predicate (car goal))
		(arity (length (cdr goal))))
	    (cond ((consp predicate)
		   (compile-body (car body) bindings cont cut-tag env))
	      ;;; a list of goals is assumed to be an and

		  ((eq predicate 'not)
	      ;;; not is deterministic it does not return bindings
		   
		   `((unless ,@(compile-body (cdr goal) 
					     bindings 
					     '#'true-cont
					     cut-tag
					     env)
			     ,@(compile-body (cdr body) bindings cont cut-tag env))))
	      
              ;;; and is ignored
		  
		  ((eq predicate 'and)
		   (compile-body (append (cdr goal) (rest body)) 
                                         bindings cont cut-tag env))
	      
	      ;;; exists find out just the first solution
	      ;;; check if binds the first arg ***???

		  ((eq predicate 'exists)
		   `((when ,@(compile-body (cddr goal) 
					     bindings 
					     '#'true-cont
					      cut-tag
					      env)
			   ,@(compile-body (cdr body) bindings cont cut-tag env))))

		  ((eq predicate 'holds)
		   `((let ((envs_holds)
                           (query (instantiate 
                                   ,(compile-back-term (cadr goal) bindings env)
                                   ,env)))
                       (setf envs_holds (ask-top-level query :all t))
                       (unless (eq envs_holds :fail)
                         (dolist (env_holds  envs_holds)  
                           (let ((new_holds_env 
                                  ,@(compile-body (cdr body)
                                                  bindings cont cut-tag 'env_holds)))
                             (when new_holds_env (return new_holds_env))))))))
                                 
	      ;;; fail, true, =, holds are defined as special cases.
		  
		  ((eq predicate 'true)
		   (compile-body (cdr body) bindings cont cut-tag env))
		  ((eq predicate 'fail)
		   '(nil))
		  ((eq predicate '=)
		   (let ((nenv (gensym "env")))
		   `((let (,nenv)
		       (unless (eq (setf ,nenv 
					 (unify ,@(mapcar 
						   #'(lambda (arg)
						       (compile-back-term arg bindings env))
						   (cdr goal))
						,env))
				   :fail)
			     ,@(compile-body (cdr body) bindings cont cut-tag nenv))))))
                  ((eq predicate 'asserted)
                   (let* ((goal (cadr goal))
                          (predicate (car goal))
		          (arity (length (cdr goal)))
                          (relation  
			   (let ((rel (find-or-create-relation predicate arity)))
			     (or rel 
				 (error "There is no relation with name ~A." predicate)))))
                     (make-relation-instances-in-line
		      relation (cdr goal) bindings env (cdr body) cont cut-tag)))
		  ((eq predicate 'findall)
		   (let ((nenv (gensym "env")))
		   `((let (res envs ,nenv)
		       (declare (special res envs))
		       ,@(compile-body (list (caddr goal)) bindings 
				       #'(lambda (ienv)
					   (setf res t)
					   (push ienv envs)
					   nil)
				       cut-tag env)
		       (when res 
			     (setf ,nenv (unify ,(compile-back-term (cadddr goal) 
							      bindings env)
					       (mapcar #'(lambda (ienv)
							   (instantiate ,(compile-back-term
									  (cadr goal) 
									  bindings
									  env)
									  ienv))
						       envs)
					       ,env))
			     (unless (eq ,nenv :fail)
				     ,@(compile-body (cdr body) 
						     bindings cont cut-tag nenv)))))))

              ;;; if a lisp function is deterministic and does not modify
	      ;;; the environment it is called with an empty environment
	      ;;; the computation goes on if :fail is not returned.
		  
		  (t (let*  ((relation  
			      (let ((rel (find-or-create-relation predicate arity)))
				(or rel 
				    (error "There is no relation with name ~A." predicate))))
			     (lisp-fun (lisp-fun relation))
                             (mapped (upward-mapping? relation)))
		       (cond (lisp-fun
			      (make-lisp-relation-code 
			       (cdr goal) lisp-fun bindings env (cdr body) cont cut-tag))

			     ((and *in-line*
				   (null (slot-of relation))
				   (null (get-domain-class predicate)))
			      (let ((new-pred (new-intern predicate arity)))
				(unless (or (defined-by-rule relation)
					    (fboundp new-pred))
					(make-standard-lisp-fun relation 
								new-pred 
								arity))		     
				(make-relation-instances-in-line
				 relation (cdr goal) bindings env (cdr body) cont cut-tag)))


	     ;;; Ortherwise is compiled as usual
	     ;;; and when is defined by a backward rule it is OK
	     ;;; because the right function will be defined during compilation.
	     ;;; Otherwise it is a relation or an instance
	     ;;; with no associated rules thus we must
	     ;;; define a function dealing with this.
			     (t
			      (let ((new-pred (new-intern predicate arity)))
                                (if (and (defined-by-rule relation)
					 mapped
					 (not (= arity (arity relation)))
                                         (not (slot-of relation)))
                                    (compile-body 
                                     (append 
                                      (maybe-canonicalise-frame-expression goal)
                                      (rest body)) bindings cont cut-tag env)
				  (progn 
				    (unless (or (defined-by-rule relation)
					        (fboundp new-pred))
				      (make-standard-lisp-fun relation 
							      new-pred 
							      arity))
				  `((,new-pred 
				     ,@(mapcar #'(lambda (arg)
						   (compile-back-term arg bindings env))
					       (cdr goal))
				     ,env
				     ,(build-continuation (cdr body) 
							  bindings cont cut-tag 'env))))))

			      )))))))))))

(defun make-standard-lisp-fun (relation name arity &optional (trace  *compile-trace*))
  "Define a lisp function qhich retrieves instances."
  (let ((args (make-parameters arity)))
    (let ((fun `(defun ,name (,@args env cont)
		  ,@(when (spying? (name relation))
			  `((print-with-spaces *task-level*
					       "~A- Goal: ~A, env: ~A"
					       ',(name relation) (list ,@args) env)))
		  .,(add-bindings-and-instances nil nil name
						relation args))))
      (if trace
	  (format t "~%~A" fun)
	  (format t "~%~A" name))
      (eval fun)
      (push name *compiled-rules*)
      (unless (or *nocompile*
                  (compiled-function-p (fdefinition name)))
        (compile name))
     ;;; (when (and *compile* (null (compiled-function-p name)))
     ;;;    (compile name))
      name)))

(defun build-continuation (body bindings cont cut-tag env)
  "Build the continuation that represent the body
   starting from the second body goal."
  (cond ((null body)
	  cont)
	 ((and (null (eq (car body) 'cut))
	       (eq (caar body) 'true)
	       (null (cdr body)))
	  cont)
	 ((and (null (eq (car body) 'cut)) 
	       (eq (caar body) 'true))
	  `(function (lambda (env)
		       ,@(compile-body (cdr body)
				       bindings 
				       cont 
				       cut-tag
				       env))))
	 ((and (null (eq (car body) 'cut))
	       (eq (caar body) 'fail)
	       '#'fail-cont))
	 (t 
	  `(function (lambda (env)
		       ,@(compile-body body
				       bindings 
				       cont 
				       cut-tag
				       env))))))

(defmacro call-bc-rule (goals &optional all env query-mode? ;;;;;compile 
                              (in-line t))
  (let* (;;;;;(*compile* compile)
	 (*in-line* in-line)
	 (vars (reverse (collect-variables goals)))
         ;         (evars (collect-variables (mapcar #'car env)))
         ;         (unbound-vars (set-difference vars evars))
	 (parameters (make-parameters (length vars)))
	 (bindings (mapcar #'cons vars parameters)))
    `(let (,@(mapcar #'(lambda (x y) 
			 `(,x ',y))
		     parameters vars)
           ,@(when all '(res envs))
           res1)
       ,@(when all '((declare (special res envs))))
       (setf res1
	     ,@(compile-body 
		(list goals) bindings
		(cond ((and (null all) (null query-mode?))
		       '#'true-cont)
		      (query-mode?
		       `(function (lambda (env) 
				    (format t "~2%Solution: ~A  " (instantiate ',goals env))
				    (not (y-or-n-p "More solutions?")))))
		      ((and all (null query-mode?))
		       #'(lambda (env)
			   (setf res t)
			   (push env envs)
			   nil)))
		nil
		(when env `(quote ,env))))
       ;       (compile-back-term env bindings nil))))
       ,@(cond (query-mode? nil)
	       (all '((when res envs)))
	       (t '((when res1 (list res1))))))))


;; COMPILE-BC-RULES this is the main function it take two optional arguments 
;; if the first is t the lisp code is not compiled
;; if the second is true relation-instances are compiled in-line.
	       
(defun compile-bc-rules (&optional nocompile (noinline t))
  (let ((*in-line* noinline))
    (clear-all-compiled-bc-rule-code)
    (setq *compiled-rules* nil)
    (maphash #'compile-bc-rule  *bc-rules*)
    (unless nocompile
	    (loop for name in *compiled-rules*
		  do (unless (or nocompile
                                 (compiled-function-p 
                                  (fdefinition name))    ;; this is needed because
			         (compile name)))))))              ;; there functions 
                                                            ;; which are compiled
                                                            ;; at run time
(defun clear-all-compiled-bc-rule-code ()
  (loop for name in *compiled-rules*
	do (fmakunbound name)
  (setq *compiled-rules* nil)))

;;; NEW-INTERN -- make a new interned symbol

(defun new-intern (name i &optional grid)
  "Make a new interned symbol concatenating name and i"
  (if grid  (intern (format nil "~A~A~D" name grid i))
      (intern (format nil "~A/~D" name i))))

;;; NEW-SYMBOL --make a new uninterned symbol
(defun new-symbol (name i)
  "Make a new uninterned symbol concatenating name and i"
  (make-symbol (format nil "~A/~D" name i)))


;;;LOGIC-MATCH-SPEC-INSTANCE-SLOTS ---Compiles the matching of a spec 
;;;such as (slot1 filler1 slot2.....)
;;;against a domain instance. 

(defun logic-match-spec-instance-slots (instance
					spec  ;;;<spec> = (slot1 filler1 slot2.....)
					fun-name
					env)
  (if (null spec)
    `(let ((resenv (funcall cont ,env))) (when resenv 
                                           (return-from ,fun-name (filter-env resenv))))
    (let ((nenv (gensym "env")))
      ;;;;`(let (,nenv)
      `(dolist 
         (value (get-slot-values ,instance 
                                 ,(compile-back-term (car spec) nil env)))
         (let (,nenv)
           (unless (eq (setf ,nenv (match value ,(second spec) ,env))
                       :fail)
             ,(logic-match-spec-instance-slots instance 
                                               (cddr spec) fun-name nenv))))
      )))

(defun make-head-body (temp-head temp-body bindings env cont cut-tag)
  (if (null temp-head)
      (if (null temp-body)
	  `((funcall ,cont ,env))
	(compile-body temp-body bindings cont cut-tag env))
    (let ((x (car temp-head))
	  (nenv (gensym "env")))
      `((let (,nenv)
          (unless (eq (setf ,nenv (unify ,(car x) 
				      ,(compile-back-term (cadr x) bindings env) 
				      ,env))
		   :fail)
	       ,@(make-head-body (cdr temp-head) temp-body bindings nenv cont cut-tag)))
      ))))

;(mapcar #'(lambda (x) (list 'logic-unify (car x) 
; (compile-back-term (cadr x) bindings)))


(defun make-slot-of-in-line (class instance-var spec fun-name)
  (let ((nenv (gensym "env")))
    ;;;;`(let (,nenv)
        `(dolist (instance (get-direct-instances ,class))
	   (let (,nenv)
	     (unless (eq (setf ,nenv 
			       (match (name instance) ,instance-var env)) 
			 :fail)
		     ,(logic-match-spec-instance-slots 'instance 
						       spec 
						       fun-name
						       nenv))
	     ))))

(defun make-slot-of-code (class instance-var spec fun-name)
  (let ((nenv (gensym "env")))
  ;;;; `(let (,nenv)
      `(dolist (instance (get-direct-instances ,class))
	   (let (,nenv)
	     (unless (eq (setf ,nenv 
			       (match (name instance) ,instance-var env)) 
			 :fail)
		     ,(logic-match-spec-instance-slots 'instance 
						       spec 
						       fun-name
						       nenv))
	     ))))


(defun make-domain-class-code (class instance-var spec fun-name)
  (let ((nenv (gensym "env")))
  ;;;; `(let (,nenv)
       `(dolist (instance (get-all-instances ,class))
	   (let (,nenv)
	     (unless (eq (setf ,nenv 
			       (match (name instance) ,instance-var env)) 
			 :fail)
		     ,(logic-match-spec-instance-slots 'instance 
						       spec 
						       fun-name
						       nenv))
	     ))))

(defun make-lisp-relation-code (args lisp-fun bindings env body cont cut-tag)
  `((let ((envs (funcall ,lisp-fun
			 ,@(mapcar #'(lambda (arg)
				       (compile-back-term 
					arg bindings env))
				   args)
			 ,env)))
      (unless (eq envs :fail)
	      (dolist (lenv envs)
		      (when ,@(compile-body body bindings cont cut-tag 'lenv)
			    (return (filter-env lenv))))))))

(defun make-relation-instances-in-line (relation args bindings env body cont cut-tag)
  `((when (relation-instances ,relation)
     (let ((cargs ,(compile-back-term args bindings env)))
      (dolist (instance (relation-instances ,relation))
	    (let (nenv)
	      (unless (eq (setf nenv (match (args instance) 
					    cargs
					    ,env))
			  :fail)
		      (when ,@(compile-body body bindings cont cut-tag 'nenv)
			    (return (filter-env nenv))))))))))

			      
(defun make-relation-instances-code (relation args fun-name)
  `((when (relation-instances ,relation)
     (let ((cargs ,(compile-back-term args nil 'env)))
      (dolist (instance (relation-instances ,relation))
	    (let (nenv)
	      (unless (eq (setf nenv (match (args instance) 
					    cargs
					    env))
			  :fail)
		      (let ((resenv (funcall cont nenv)))
			(when resenv (return-from ,fun-name (filter-env resenv)))))))))))



;(defun logic-uvariable? (pattern)
;  (and (symbolp pattern)
;       (char= (elt (symbol-name pattern)0)
;              #\?)
;       (char= (elt (symbol-name pattern)1)
;	      #\#)
;       ))

(defun compile-query (goals bindings env)
  `(let ((res_if ,@(compile-body (list goals) bindings '#'true-cont nil env)))
     (cond ((eq res_if t) nil)
           (t (or res_if :fail)))))




  



