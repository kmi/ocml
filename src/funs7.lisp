;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


;;;;FUNCTIONS
(defclass ocml-function (name-mixin lisp-attachment-mixin ;;;onto-spec-mixin
                                    basic-ocml-object)
  ((trace-depth :initform 0 :accessor fun-trace-depth)
   (arity :initarg :arity :initform nil :accessor arity)
   (schema :initarg :schema :initform nil :accessor schema)
   (constraint :initarg :constraint :initform nil :accessor constraint)   
   (definition :initarg :def :initform nil :accessor definition)
   (output-var :initarg :output-var :initform nil)
   ;;;;;;;(translates-to :initarg :translates-to )
   (free-vars-in-body :accessor free-vars-in-body)
   (spec :initarg :spec :initform nil)
   (body :initarg :body :initform nil :accessor body)
   (trace? :initform nil :accessor fun-trace-on?))
  (:documentation "The class of functions in OCML"))

(defmethod print-object ((instance ocml-function) stream)
  (with-slots (name)instance
    (format stream "#<~S ~S>" (type-of instance) name)))

;;;OCML-FUNCTION? ---True if <name> is the name of a OCML function.  It is false
;;;if <name> denotes a procedure
(defun ocml-function? (name)
  (if (member name *hardwired-functions*)
      name
      (let ((s (get-function name)))
        (when (and s (not (procedure-p s)))
	  (values name s)))))
           
(defun all-functions (&optional structures)
  "Returns all names of functions"
  (if structures
    (map-over-hash-table #'(lambda (name structure)
                             name ;;ignore
                             structure)
                         *defined-functions*)
    
    (map-over-hash-table #'(lambda (name structure)
                             structure ;;ignore
                             name)
                         *defined-functions*)))



(defun make-ocml-function (&rest options)
  (let ((instance (apply  #'make-instance (cons 'ocml-function options))))
    (set-free-vars-in-body instance)
    (propagate-new-def-to-sub-ontologies (name instance) instance 'function)
    instance))
     
;;;INITIALIZE-INSTANCE :AFTER OCML-FUNCTION
(defmethod initialize-instance :after ((fun ocml-function) &rest initargs)
  (declare (ignore initargs))
  (with-slots (name schema arity user-defined? lisp-fun) fun
    (let ((old-fun (get-function name)))
      (when old-fun 
        (let ((old-ontology (home-ontology old-fun))
              (source-file (car (source-files name 'ocml-function))))
            (cond ((eq old-ontology *current-ontology*)
                   (unless (equal (and source-file
                                       (translate-logical-pathname source-file))
                                  (and (current-file)(translate-logical-pathname (current-file))))
                     (if source-file
                       (ocml-warn "Redefining function ~S, previously defined in file ~a"
                                  name source-file)
                       (ocml-warn "Function ~s redefined in file ~a"
                                  name (current-file)))))
                  (t
                   (ocml-warn 
                    "Redefining function ~S, previously defined in ontology ~s" 
                    name (name 
                          old-ontology)))))))
      
      ;;;(warn "Redefining function ~S"name))
    #-:lispworks (record-source-file name (type-of fun))
   ; #+(or allegro lispworks)(record-source-file name
;                                     (if (eq (type-of fun) 'ocml-function)
;                                       'def-function
;                                       'def-procedure))
    #+(or allegro lispworks)(ocml-record-source-file
                             name
                             (if (eq (type-of fun) 'ocml-function)
                                 'def-function
                                 'def-procedure))
    (enforce-arity-schema-consistency fun name schema arity)
    (when lisp-fun
      (setf user-defined? t))
    (add-to-functions-directory name fun)))
    ;;;(maybe-update-definition-in-super-ontologies fun)))

;;;SET-FREE-VARS-IN-BODY
(defmethod set-free-vars-in-body  ((fun ocml-function))
   (with-slots (schema body free-vars-in-body) fun
     (when body
       (setf free-vars-in-body
             (set-difference (collect-variables body)
                             schema)))))



;;;MAYBE-RENAME-SCHEMA-AND-BODY ---If an ocml expression is specified for a function,
;;;then we rename its variables to avoid possible name clashes.
(defmethod maybe-rename-schema-and-body  ((fun ocml-function))
   (with-slots (schema body) fun
     (when body
       (multiple-value-setq (schema body)
         (rename-args-and-body schema body)))))

(defun add-to-functions-directory (name instance)
  (setf (gethash name *defined-functions*) instance))

(defun remove-all-functions ()
  (clrhash *defined-functions*))

(defun get-function (name)
  (gethash name *defined-functions*))

(defun remove-function-internal (name)
  (remhash name *defined-functions*))


;;;REMOVE-FUNCTION  - Top level method for removing a function
(defun remove-function (name)
  "Remove function NAME."
  (let ((function (get-function name)))
    (when function
      (remove-function-in-all-ontologies name function))))

(defun remove-function-in-all-ontologies (name function)
  (remove-function-internal name)
  (maybe-fetch-definition-from-super-ontologies *current-ontology*
                                              'ontology-functions
                                              name   
                                              *current-ontology*)
  (remove-def-from-dependent-ontologies name function 'function))

(defun rename-function (old-name new-name)
  (let ((function (get-function old-name)))
    (cond( function
           (remove-function-in-all-ontologies old-name function)
           (setf (name function) new-name)
           (add-to-functions-directory new-name function)
           (propagate-new-def-to-sub-ontologies new-name function 'function))           
         (t
          (error "~s is not a function" old-name)))))

(defun fun-termp (thing)
  (and (listp thing)
       (get-function (car thing))))

(defun trace-this-fun (ocml-fun)
  (setf (fun-trace-on? ocml-fun)t))

(defun untrace-this-fun (ocml-fun)
  (setf (fun-trace-on? ocml-fun)nil))


          
      

;;;APPLY-OCML-F-OR-P  ---Applies a function or procedure to a list of arguments.
;;;If a lisp attachment is given, this is used.  Otherwise the function definition is used.
;;;If neither exists, an error is signalled
(defmethod apply-ocml-f-or-p ((s ocml-function) args
                              &optional env (eval-fun #'ocml-eval-fun) (eval-args? t) 
                              &aux result  new-args)
  (with-slots (lisp-fun  body name free-vars-in-body schema trace? user-defined?) s
    (setf result 
          (multiple-value-list
          (cond (lisp-fun
                 (setf new-args (if eval-args? 
                                  (new-evaluate-args args env eval-fun)
                                  args))
                 (when trace?
                   (incf *trace-depth-counter*)
                   (print-with-spaces *trace-depth-counter*
                                      "Enter ~S with arguments ~{~s ~}in env ~S"
                           name new-args (relevant-env new-args env)))
                 (apply-lisp-fun  lisp-fun new-args env user-defined? ))
                (body 
                 (let ((l  (length schema)))
                   (multiple-value-bind (vars new-body)
                                        (rename-args-and-body (append schema free-vars-in-body) body)
                     (setf new-args (if eval-args? 
                                      (new-evaluate-args args env eval-fun)
                                      args))
                     (Let* ((nenv (multiple-variable-bind (subseq vars 0 l)
                                                          new-args
                                                          env)))
                       (when trace?
                         (incf *trace-depth-counter*)
                         (print-with-spaces *trace-depth-counter*
                                          "Enter ~S with arguments ~{~s ~}in env ~S"
                                          name new-args 
                                          (relevant-env new-args env)))                         
                       (funcall eval-fun new-body nenv)))))
                 
                (t
                 (error "No definition for ~s ~s"
                        (if (procedure-p s)"procedure" "function")
                        name)))))
    (when trace? 
       ;;;;;(and trace?
       ;;;;;;        (not (procedure-p s)))
      (print-with-spaces *trace-depth-counter*
                         "Exit ~S -> ~S" name (car result))
      (decf *trace-depth-counter*) )
    (values-list result)))






(defun apply-lisp-fun ( lisp-fun args env  user-defined? );;;; eval-args?)
  (let ((*current-environment* env))
    (apply lisp-fun 
           (if user-defined?
             args
             (cons env args)))))



(defun ocml-eval-var-or-error (var env)
  (multiple-value-bind (flag value)
      (ocml-eval-var var env)
    (if flag
	value
	(error "OCML variable ~s is unbound" var))))


;;;OCML-EVAL-VAR
(defun ocml-eval-var (var &optional env)
  (multiple-value-bind (flag value)
                       (binding-of var env)
    (when flag
      (if (atom value)
        (values flag (lookup-or-self value env))
        (if (or (lambda-exp? value)
                (kappa-exp? value))
          (values flag value)
          (values flag 
                  (cons (lookup-or-self (car value) env)
                        (lookup-or-self (cdr value) env))))))))
                     
               
    

;;;OCML-EVAL-FUN  ---Top level function to evaluate a function expression
(defun ocml-eval-fun (term &optional env);;;;; error-on-unbound-vars?)
  (cond ((consp term)
         (multiple-value-bind (flag s)
             (ocml-function? (car term))
           (if flag
               (if s
                   (apply-ocml-f-or-p s (cdr term) env)
                   (ocml-eval-primitive-fun (car term) (cdr term) env))
	       (if (lambda-exp? term)
		   term
		   (error "~S is not an OCML function" (car term))))))
	((variable? term)
         (ocml-eval-var-or-error term env))
        (t
         term)))

(defun ocml-eval-primitive-fun (fun args &optional env)
  (case fun
     (quote (car args))
     (if (evaluate-if-form args env))
     (cond (evaluate-cond-form args env))
     (in-environment (evaluate-in-env-form args env))
     (findall (evaluate-findall-form args env))
     (setofall (evaluate-setofall-form args env))
     (the (evaluate-the-form args env))
     (eval (ocml-eval-gen (ocml-eval-gen (car args) env)env))
     (apply (ocml-apply-internal 
                   (ocml-eval-fun (car args) env)
                   (ocml-eval-fun (second args) env)
                   env))
     (call (ocml-apply-internal 
                   (ocml-eval-fun (car args) env)
                   (new-evaluate-args (cdr args) env)
                   env))
     ;;;;(call (evaluate-call-form args env))
     ;;;;;(rcall (evaluate-rcall-form args env))
     ;;;;;;(set-of (evaluate-set-of-form args env))
     (t                                    
      (error "~S is not an OCML function (current env is ~s)" fun env))))
  
;;;OCML-EVAL-FUN-TERM---Applies an OCML function to its arguments.
;;;The argument fun can be either the name of a function or a lambda expression.
(defun ocml-eval-fun-term (fun args &optional env (eval-fun #'ocml-eval-fun))
  (cond ((lambda-exp? fun)
         (apply-lambda-exp fun args env eval-fun))
        ((and (eq eval-fun #'procedure-eval)
              (member fun *hardwired-procedures*))
         (apply-primitive-procedure fun args env eval-fun))
        ((member fun *hardwired-functions*)
	 (ocml-eval-primitive-fun fun args env))
        (t
	 (let ((fun-structure (get-function fun)))
	   (if fun-structure
		  ;;;;;  (not (procedure-p fun-structure)
	       (apply-ocml-f-or-p fun-structure args env)
               (if (eq eval-fun #'ocml-eval-fun)
	         (error "~S is not an OCML function (current env is ~s)" fun env)
                 (error "~S is not an OCML procedure (current env is ~s)" fun env)))))))
                 

;;;APPLY-LAMBDA-EXP
(defun apply-lambda-exp (lambda args  &optional env (eval-fun #'ocml-eval-fun) (eval-args? t)
                                &aux result cont)
  (destructuring-bind (ignore lambda-args &rest body)
                      lambda
    (declare (ignore ignore))
    (when lambda-args
      (multiple-value-setq (lambda-args body)
        (rename-args-and-body lambda-args  body))
      (setf env (multiple-variable-bind lambda-args 
                                        (if eval-args?
                                          (new-evaluate-args args env)
                                          args)
                                        env))
      (dolist (form body)
        (multiple-value-setq (result cont)
          (funcall eval-fun form env))))) ;;;;(instantiate form env))))))
  (values result cont))

(defun lambda-exp? (thing)
  (and (listp thing)
       (eq (car thing) 'lambda)
       (listp (cdr thing)) ;;;To avoid problems with dotted pairs
       (= (length thing) 3)
       
       (schema? (second thing))))
      ;;;;;; (term (third thing))))


;;;OCML-APPLY-INTERNAL ---Calls the OCML interpreter to apply a 
;;;function to a list of arguments.
(defun ocml-apply-internal (fun args &optional env (eval-fun #'ocml-eval-fun ))
  (cond ((lambda-exp? fun)
         (apply-lambda-exp fun args env eval-fun nil))
        ((member fun *hardwired-functions*)
         (error "APPLY cannot be called on a primitive function..when evaluating ~S in ~S"
                (cons fun args)
                (relevant-env (cons fun args) env)))
        ((and (eq eval-fun #'procedure-eval)
              (member fun *hardwired-procedures*))
         (error "APPLY cannot be called on a primitive procedure evaluating ~S in ~S"
                (cons fun args)
                (relevant-env (cons fun args) env)))
       (t
	 (let ((fun-structure (get-function fun)))
	   (if fun-structure
		  ;;;;;  (not (procedure-p fun-structure)))
	       (apply-ocml-f-or-p fun-structure args env eval-fun nil)
	       (if (eq eval-fun #'ocml-eval-fun )
                 (error "can't find function ~s" fun)
                 (error "can't find function or procedure  ~s" fun)))))))



;;;EVALUATE-IF-FORM --Evaluates If expressions.  If the test condition is satisfied
;;;but no 'then' branch is found then :nothing is returned.  The same applies if the
;;;test condition fails and there is no 'else' branch
(defun evaluate-if-form (args env &optional (eval-fun #'ocml-eval-fun))
  (destructuring-bind (goal &optional (then *nothing* then?) (else *nothing* else?))
      args
    (let ((nenv (ask-top-level goal :env env)))
      (if (eq nenv :fail)
          (if else?
            (funcall eval-fun  else env)
            *nothing*)
          (if then?
              (funcall eval-fun then nenv)
              *nothing*)))))

(defun evaluate-cond-form (args env &optional (eval-fun #'ocml-eval-fun))
  (loop for clause in args
        for goal = (car clause)
        for new-env = (ask-top-level goal :env env)
        while (eq new-env  :fail)
        finally
        (return
         (if (eq new-env :fail)
             *nothing*
             (funcall eval-fun (second clause) new-env)))))

(defun evaluate-loop-form (args env &optional (eval-fun #'procedure-eval))
  (catch 'ocml-control
    (destructuring-bind (for var in exp do &rest body)args
      (cond ((and (eq for 'for)
                  (eq in 'in)
                  (eq do 'do))
             (loop with list = (ocml-eval-fun exp env)
                   for el in list
                   do
                   (loop for action in body
	                 do
	                 (funcall eval-fun ;;;;;task-level-eval
                                  action
				  (bind var el env)))
                   finally (return :nothing)))
            (t
             (error "Bad syntax for Loop expression..when evaluating ~S"
                    (cons 'loop args)))))))


(defun evaluate-in-env-form (args env &optional (eval-fun #'ocml-eval-fun) &aux (new-env env))
  (when (and (cddr args)
             (not (eq eval-fun #'procedure-eval)))
    (warn "Incorrect body to in-environment form...ignoring additional expressions, ~%~{~S ~}"
          (cddr args)))
  (loop for binding in (car args)
        do
        (setf new-env (bind (car binding)(funcall eval-fun (cdr binding)new-env) new-env))
	finally
        (return 
         (loop for form in (cdr args)
              for result = (funcall eval-fun form new-env)
              finally
              (return result)))))


(defun  evaluate-findall-form (args env )
  (findall (car args)(second args) env))


(defun  evaluate-setofall-form (args env )
  (setofall (car args)(second args) env))

(defun  evaluate-the-form (args env )
  (findany (car args)(second args) env))

(defun findany-aux (exp goal env nenv cont)
    (if (eq nenv :fail)
        :nothing
        (values (instantiate exp nenv)
                #'(lambda ()
                    (multiple-value-bind (nenv2 cont2)
                       (funcall cont)
                      (findany-aux exp goal env nenv2 cont2))))))
            
(defun maybe-evaluate-term (term &optional env)
  "evaluates a term if this is a functional application"
  (cond ((consp term)
         (if (ocml-function? (car term))
           (ocml-eval-fun term env)
           term))
        (t
         term)))

(defun eval-functional-arg-or-instantiate-rel (arg env)
  (cond 
        ((listp arg)
         (if (ocml-function? (car arg))
             (ocml-eval-fun arg env)
             (if (variable? (car arg))
		 (cons (instantiate (car arg)env)
                       (cdr arg))
                 arg)))
        ((variable? arg)
         (ocml-eval-var-or-error arg env))
        (t
         arg)))

(defun eval-functional-arg (arg &optional env)
  (cond 
        ((listp arg)
         (if (ocml-function? (car arg))
             (ocml-eval-fun arg env)
             arg))
        (t
         arg)))

(defun new-evaluate-args (args env &optional (fun #'ocml-eval-fun))
  (let ((*depth* (1+ *depth*)))
    *depth*
  (mapcar #'(lambda (arg)
              (funcall fun arg env))
          args)))


;;;EVAL-OR-INSTANTIATE-ARGS ---This is called when assertions are added or deleted
;;;to ensure that arguments are evaluated if necessary, or instantiated if they 
;;;don't need to be evaluated
(defun eval-or-instantiate-args (args env )
  (mapcar #'(lambda (arg)
	      (cond ((listp arg)
                     ;;;;(if 
		     ;;;;;;(eq (car arg) *ocml-eval-macro-symbol*)
		     ;;;;;;;(ocml-eval2 (second arg) env)
		     (if (ocml-function? (car arg))
			 (ocml-eval-fun arg env)
			 (instantiate arg env)))
                    (t
		     (instantiate arg env))))
	  args))


;(defmacro define-function-internal (name schema arrow var
;                                         documentation &rest options)
;  (unless (eq arrow '->)
;    (setf options
;          (if documentation
;            (append (list var documentation)
;                       options)
;            (when arrow
;              (list var)))
;          documentation arrow))
;  (multiple-value-bind (name schema documentation options)
;             (parse-define-function-form name schema documentation options)
;           `(funcall #'make-ocml-function
;                     :name ',name
;                     :schema ',schema
;                     :output-var ',var
;                     :documentation  ,documentation
;	             ,@(mapcar #'(lambda (x)
;                                   (list 'quote x))
;			       options))))

;;;Redefined - 6/11/01 - Enrico
(defmacro define-function-internal (name schema arrow var
                                         documentation &rest options)
  (cond ((eq arrow '->)
         (unless (stringp documentation)
           (when documentation
             (setf options (cons documentation options)
                   documentation nil))))
        
        ((stringp arrow)
         (if var
           (setf options (append (list var documentation)
                                 options)
                 documentation arrow
                 var nil)
           (setf documentation arrow)))
        (arrow
         (setf options
               (append (list arrow var)
                       (when documentation
                         (list documentation))
                       options)
               documentation nil
               var nil)))
  ;;;;(multiple-value-bind (name1 schema1 documentation1 options1)
  ;;;;;;;           (parse-define-function-form name schema documentation options)
           `(funcall #'make-ocml-function
                     :name ',name
                     :schema ',schema
                     :output-var ',var
                     :documentation  ,documentation
	             ,@(mapcar #'(lambda (x)
                                   (list 'quote x))
			       options)))






;;;OCML-PROCEDURE
(defclass ocml-procedure (ocml-function)
  ((goal :initarg :goal :initform nil))
  (:documentation "The class of procedures in OCML"))

(defun procedure? (name)
  (or (member name *hardwired-procedures*)
      (Let ((f (get-function name)))
        (and f
             (procedure-p f)))))

(defun function-or-procedure? (name)
  (or (member name *hardwired-procedures*)
      (member name *hardwired-functions*)
      (get-function name)))



;(defun ocml-procedure? (name)
;  (Let ((f (get-function name)))
;    (when f
;      (procedure-p f))))

(defun procedure-p (x)
  (typep x ' ocml-procedure))

(defmacro define-procedure-internal (name schema documentation &rest options)
  (multiple-value-bind (name schema documentation options)
      (parse-define-function-form name schema documentation options)
    `(funcall #'make-ocml-procedure
              :name ',name
              :schema ',schema
              :documentation  ,documentation
	      ,@(mapcar #'(lambda (x)
                            (list 'quote x))
			options))))

(defun make-ocml-procedure (&rest options)
  (let ((instance (apply  #'make-instance (cons 'ocml-procedure options))))
    ;;;;(record-source-file (name instance)'ocml-procedure)
    (set-free-vars-in-body instance)
    (propagate-new-def-to-sub-ontologies (name instance) instance 'function)
    instance))