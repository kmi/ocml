;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defmacro def-class (name &optional (superclasses)  instance-var  documentation
                          class-slots &rest relation-spec)
  `(define-domain-class ',name ',superclasses ',instance-var  ',documentation
     ',class-slots ',relation-spec))

#+:lispworks 
(editor::setup-indent 'def-class 0 2)

(defmacro def-domain-class (name superclasses &optional instance-var  documentation
                                 class-slots &rest relation-spec)
  `(define-domain-class ',name ',superclasses ',instance-var  ',documentation
     ',class-slots ',relation-spec))

#+:lispworks 
(editor::setup-indent 'def-domain-class 0 2)

(defmacro def-domain-instance (name parent &optional documentation
                                    slots)
  `(define-domain-instance ', name ',parent ',documentation ',slots))

#+:lispworks 
(editor::setup-indent 'def-domain-instance 0 2)

(defmacro def-instance (name parent &optional documentation
                             slots)
  `(define-domain-instance ', name ',parent ',documentation ',slots))

#+:lispworks 
(editor::setup-indent 'def-instance 0 2)

(defmacro def-relation (name schema &optional  documentation &rest options)
  `(define-relation-internal ',name  ',schema ',documentation ',options))

#+:lispworks 
(editor::setup-indent 'def-relation 0 2)

(defmacro def-relation-instances (&optional  documentation &rest instances)
  `(def-relation-instances-internal ',documentation ',instances))

#+:lispworks 
(editor::setup-indent 'def-relation-instances 0 2)

(defmacro def-function (name  schema &optional arrow var  documentation &rest options)
  `(define-function-internal ,name  ,schema ,arrow ,var ,documentation ,@options))

#+:lispworks 
(editor::setup-indent 'def-function 0 2)

(defmacro def-operator (name  &optional schema documentation &rest options)
  `(define-relation-internal ',name  ',schema ',documentation ',options))

#+:lispworks 
(editor::setup-indent 'def-operator 0 2)

(defmacro def-procedure (name  &optional schema documentation &rest options)
  `(define-procedure-internal ,name  ,schema ,documentation ,@options))

#+:lispworks 
(editor::setup-indent 'def-procedure 0 2)

(defmacro def-rule (name &optional documentation &rest args)
  `(def-rule-internal ',name ',documentation ',args))

#+:lispworks 
(editor::setup-indent 'def-rule 0 2)

(defmacro def-task (name superclasses &optional instance-var  documentation
                         class-slots &rest relation-spec)
  `(def-task-internal ',name ',superclasses ',instance-var  ',documentation
     ',class-slots ',relation-spec))

#+:lispworks 
(editor::setup-indent 'def-task 0 2)

(defmacro def-axiom (name &optional documentation axiom)
  `(def-axiom-internal ',name ',documentation ',axiom))

#+:lispworks 
(editor::setup-indent 'def-axiom 0 2)

(defmacro def-ontology (name &optional documentation &rest options)
  `(def-ontology-internal ',name ',documentation ',options))


#+:lispworks 
(editor::setup-indent 'def-ontology 0 2)

(defmacro ensure-ontology (name &optional type pathname)
  `(ensure-ontology-internal ',name ',type ',pathname))

(defmacro def-relation-mapping (relation direction documentation &rest clauses)
  `(def-rel-mapping-internal ',relation ',direction ',documentation ',clauses))


#+:lispworks 
(editor::setup-indent 'def-relation-mapping 0 2)

;;;from mapping4.lisp to help with compilation 
;;;john domingue march '98
(defun default-upward-naming-function (domain-name method-class)
  (read-from-string 
   (string-append
    (string method-class)
    "-"
    (string domain-name))))

;;;DEF-UPWARD-CLASS-MAPPING ---This creates instances of method-class corresponding
;;;to instances of domain class
(defmacro def-upward-class-mapping (domain-class 
                                    method-class 
                                    &key (name-function #'default-upward-naming-function)
                                    (explicit-link t)
                                    (mapping-relation *default-mapping-relation*))
  `(def-upward-class-mapping-internal ',domain-class ',method-class ,name-function
     ,explicit-link ',mapping-relation))


#+:lispworks 
(editor::setup-indent 'def-upward-class-mapping 0 2)

;;;TELL
(defmacro tell (exp &optional documentation)
  `(tell1 ',exp ,documentation))

(defun tell1 (exp &optional documentation env &aux new-form fexp)
  (multiple-value-bind (pred-structure args original-form)
                       (parse-assertion exp env)
    (setf new-form (cons (name pred-structure) args))
    (when (tracing-this-assertion? new-form)
      (print-with-spaces (1+ *task-level*) "Asserting ~S in ontology ~s" new-form
                         *current-ontology*))
    (cond ((setf fexp (downward-add-exp pred-structure))
           (trigger-downward-mapping-exp fexp args))
          ((slotp pred-structure)
           (maybe-add-slot-assertion pred-structure args original-form))
          ((get-domain-class (name pred-structure))
           ;;;; (= 1 (length args)))
           (define-domain-instance  (car args) (car original-form)
             (or documentation "")
             (pairify (cdr args))))
          (t
           (maybe-add-relation-instance pred-structure args original-form documentation)))
    new-form))



(defun add-assertion (predicate args)
  (tell1 (cons predicate args)))

(defun add-multiple-assertions (predicate args)
  (loop for arglist in args
        do
        (add-assertion predicate arglist)))


;;;UNASSERT ---Deletes all assertions matching <exp>, which has the format 
;;;(<pred><term1.>....)
;;;Terms can contain variables
(defmacro unassert (exp)
  `(unassert1 ',exp))


;;;UNASSERT1
(defun unassert1 (exp &optional env ;;;;;no-checks 
                      &aux class fexp)
  (multiple-value-bind (pred-structure args )
                       (parse-assertion-to-delete exp env)
    (when pred-structure
      (cond ((setf fexp (downward-remove-exp pred-structure))
             (trigger-downward-mapping-exp fexp args))
	    ((slotp pred-structure)
             (maybe-delete-slot-assertion pred-structure args exp)) ;;;no-checks))
            ((setf class (get-domain-class (car exp)))
             (if (= 1 (length args))
               (remove-domain-instance-gen  (car args) (car exp) class)
               (remove-instances-matching-spec class (car args)(cdr args) (name pred-structure))))
	    (t
             (remove-relation-instance-gen pred-structure args
                                           (some #'variable? args)))))))


;;;COMPILE-RULE-PACKETS ---Compiles the forward chaining rules in the specifies packets.
;;;By default it compiles all rules in all packets
(defun compile-rule-packets (&optional (packets :all))
  (compile-fc-rules  (if (eq packets :all)
                       (merge-rules (all-rule-packets))
                       (merge-rules (mapcar #'get-rule-packet packets)))))

;;;;(defun COMPILE-ALL-FUNCTIONS-&-PROCEDURES (&optional verbose force-p)
;;;;  (compile-all-funs&procs verbose force-p))


;;;RUN ---Invokes the forward chainer
(defun run (&optional (packets :all))
  (run-fc-rules packets))


;;;ENABLE-FC-WATCHER-MODE
;;;Top level function to enable 'watcher mode'.  All packets 
;;;given as input are declared 'active', rules are compiled
;;;and instantiations are fired.  In addition, the flag 
;;;*fc-in-watcher-mode* is set to T, so that  new instantiations 
;;;will be fired as soon as they are created

(defun enable-fc-watcher-mode (&optional (packets :all))
  (let* ((packets (if (eq packets :all)
                    (all-rule-packets)
                    (mapcar #'get-or-create-packet packets)))
         (rules (merge-rules packets)))
    (mapcar #'(lambda (packet)
                (setf (rule-packet-active? packet) t))
            packets)
    (compile-fc-rules rules)
    (setf *fc-in-watcher-mode* t)
    (do-interpreter-cycle rules)))


;;;DISABLE-FC-WATCHER-MODE
;;;Disables 'watcher mode'

(defun disable-fc-watcher-mode ()
  (setf *fc-in-watcher-mode* nil))


;;;EXECUTE-APPLICATION
(defun execute-application (appl)
  (setf *current-application* appl)
  (apply-method (find-current-instance (the-slot-value appl 'uses-method) 
                                       'problem-solving-method)
                appl))

(defmethod apply-method (method appl)
  (ocml-warn "No specific APPLY-METHOD defined for method ~s on application ~s"
             method appl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Simple Tracing


;;;FORWARD CHAINING
(defun trace-fc ()
  ;; (trace-assertions)
  (setf *trace-fc* t))

(defun untrace-fc ()
  (setf *trace-fc* nil))

;;;;;;;;;;;;;;;;;;;;;

;;;BACKWARD CHAINING
(defmacro spy (&rest preds)
  `(if ', preds ;;;;;(eq preds :all)
     (unless (eq *spied-predicates* :all)
       (Setf *spied-predicates*(union ', preds *spied-predicates*)))
     (setf *spied-predicates* :all)))


(defmacro  nospy (&rest preds)
  `(if ',preds
     (if (listp *spied-predicates*)
       (setf *spied-predicates* (Set-difference *spied-predicates* ',preds))
       ;;;We are currently spying all predicates, and the argument to nospy specifies
       ;;;which ones shouldn't be spied anymore.  This means we should spy all the
       ;;;existing predicates not explicitly listed in preds. This is too much effort.
       ;;;Let's just do a global nospy
       (setf *spied-predicates* nil))
     (Setf  *spied-predicates* nil)))


;;;;;;;;;;;;;;;;;;;;;
;;;ASSERTIONS
(defmacro trace-assertions (&rest preds)
  `(if (eq ',preds :all)
     (setf *traced-assertions* :all)
     (if ',preds
       (unless (eq *traced-assertions* :all)
         (loop for pred in ',preds
               do
               (pushnew pred *traced-assertions*)))
       (setf *traced-assertions* :all))))

(defmacro untrace-assertions (&rest preds)
  `(if (or (null ',preds)
           (eq ',preds :all))
     (setf  *traced-assertions* nil)
     ;;we are untracing some preds 
     (cond ((eq  *traced-assertions* :all)
            (warn "untracing all assertions")
            (setf  *traced-assertions* nil))
           (t 
            ;; *traced-assertions* is a list
            (loop for pred in ',preds
                  do
                  (setf *traced-assertions*
                        (remove pred *traced-assertions*)))))))

(defun tracing-this-assertion? (exp)
  (or (eq *traced-assertions* :all)
      (member (car exp) *traced-assertions*)))

;;(defun trace-assertions (&rest preds)
;;  (setf *trace-assertions* t))

;;(defun untrace-assertions ()
;;  (setf *trace-assertions* nil))

;;;FUNCTIONS
(defmacro trace-function (&rest funs)
  `(dolist (fun ',funs)
     (trace-this-fun (get-function fun))))

(defmacro untrace-function (&rest funs)
  `(dolist (fun ',funs)
     (untrace-this-fun (get-function fun))))

(defun untrace-all-functions ()
  (maphash #'(lambda (key fun)
               (declare (ignore key))
               (setf (fun-trace-on? fun)nil))
           *defined-functions*))

;;;TASKS
(defmacro trace-tasks (&rest tasks)
  `(if (or (null ',tasks)
           (eq ',tasks :all))
     (setf *traced-tasks* :all)
     (when ',tasks
       (unless (eq *traced-tasks* :all)
         (loop for task in ',tasks
               do
               (trace-this-task task))))))

(defmacro untrace-tasks (&rest tasks)
  `(if (or (null ',tasks)
           (eq ',tasks :all))
     (setf  *traced-tasks* nil)
     ;;we are untracing some tasks 
     (cond ((eq  *traced-tasks* :all)
            (warn "untracing all tasks")
            (setf  *traced-tasks* nil))
           (t 
            ;; *traced-tasks* is a list
            (loop for task in ',tasks
                  do
                  (untrace-this-task task))))))

;;;UNTRACE-ALL
(defun untrace-all ()
  (untrace-tasks)
  (untrace-assertions)
  (untrace-all-functions)
  (nospy)
  (untrace-fc))


;;;ENABLE-CONSTRAINT-CHECKING
(defun enable-constraint-checking ()
  (setf *check-constraints* t *check-cardinality* t))

;;;DISABLE-CONSTRAINT-CHECKING
(defun disable-constraint-checking ()
  (setf *check-constraints* nil  *check-cardinality* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;OCML-EVAL ---Top level macro to evaluate a control or function expression
(defmacro ocml-eval (term &optional env)
  `(ocml-eval-gen ',term ,env))

;;;OCML-EVAL-GEN --Top level function to evaluate a control or function expression
(defun ocml-eval-gen (term &optional env)
  (cond ((consp term)
	 (if (procedure? (car term))
           (procedure-eval term env)
           (ocml-eval-fun term env)))
	((variable? term)
         (ocml-eval-var-or-error term env))
        (t
         term)))

;;;OCML-EVAL-FUN-EXPR- This evaluates a function expression (not a 
;;;control expresssion!)
(defun ocml-eval-fun-exp (fun-exp  &optional env)
  (ocml-eval-fun fun-exp  env))


;;;OCML-CALL  ---Invokes a function on some arguments.
(defun ocml-call (fun &rest args)
  (ocml-eval-fun-term fun args))

(defun ocml-apply (fun  args &optional env (eval-fun #'ocml-eval-fun))
  (ocml-eval-fun-term fun args env eval-fun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro dc (class-name &optional local-defs-only?)
  `(describe-class ',class-name ,local-defs-only?))

(defmacro di (instance-name &optional class-name deduce-all-values?)
  `(describe-instance ',instance-name :class-name ',class-name 
                      :deduce-all-values? ,deduce-all-values?))


(defun direct-instances (class)
  (let ((class-structure (get-ocml-class class)))
    (if class-structure
      (get-direct-instances class-structure)
      (error "~S is not a domain class" class))))

(defun all-instances (class)
  (get-all-instances (get-ocml-class class)))

(defun all-current-instances (class)
  (get-current-instances  (get-ocml-class class)))

(defun all-current-direct-instances (class)
  (get-current-direct-instances  (get-ocml-class class)))

;; (append (direct-instances class)
;;         (mapcan* #'(lambda (x)
;;                      (all-instances (name x)))
;;                 (direct-subclasses class (get-domain-class class)))))

;;;INSTANCE-OF? ---True if name is an instance (not just a direct instance!!!) of  class.  
(defun instance-of? (name class-name)
  (find-all-current-instances-of-class-named-x name class-name))



;;;DIRECT-INSTANCE-OF?
(defun direct-instance-of? (name class)
  (find-current-direct-instance (get-domain-class class) name))

;;;***************************************************************

;;;HOLDS?  ---This is useful to check whether an expression is true in the OCML KB.
;;;It returns NIL if the expression is false.  Otherwise it returns T if no binding
;;;was created or the resulting environment.
(defun goal-holds? (exp &optional env)
  (Let ((result (ask-top-level exp :env env)))
    (cond ((eq  result :fail)
           nil)
          ((null result)
           t)
          (t
           result))))

(defun holds? (rel &rest args)
  (Let ((result (ask-top-level `(holds ,rel ,@args))))
    (cond ((eq  result :fail)
           nil)
          ((null result)
           t)
          (t
           result))))

(defun holds-in-env? (rel args &optional env)
  (Let ((result (ask-top-level `(holds ,rel ,@args) :env env)))
    (cond ((eq  result :fail)
           nil)
          ((null result)
           t)
          (t
           result))))

;;;FINDALL --Returns all the correct matches for a goal and then returns the list of
;;;the instantiations of <exp> in the resulting environments.  FINDALL returns
;;;NIL (the empty list) if no match is found
(defun findall (exp goal &optional env)
  (Let ((envs (ask-top-level goal :all t :env env)))
    (if (eq envs :fail)
      nil
      (mapcar #'(lambda (env)
                  (instantiate exp env))
              envs))))

;;;SETOFALL ---Like findall, but makes sure there are no duplicates
(defun setofall (exp goal &optional env)
  (remove-duplicates (findall exp goal env)
                     :test #'equal))


;;;FINDANY  --Find one (and only one)  correct match for a goal and then returns 
;;;the instantiation of <exp> in the resulting environment (+ the continuation).
;;;FINDANY returns :NOTHING if no match is found
(defun findany (exp goal &optional env)
  (multiple-value-bind (nenv cont)
                       (ask-top-level goal :env env)
    (findany-aux exp goal env nenv cont)))

(defun findany-gen (exp goal &optional env)
  (loop with result
        with cont
        initially (multiple-value-setq (result cont)
                    (findany exp goal env))
        
        do
        (cond((eq result :nothing)
              (return :nothing))
             (t
              (print result)
              (multiple-value-setq (result cont)
                (funcall cont))))))

;;;ASK
(defmacro ask (query &optional all? env)
  `(ask-top-level ',query :all ',all? :env ,env :query-mode t))

;;;ESTABLISH = ASK, GET result, and  TELL it
(defmacro establish (query)
  `(establish1  ',query))

(defun establish1 (goal &optional env)
  (when (member (car goal)
                '(exists forall not or))
    (error "Can't assert a pattern such as ~S" goal))
  ;(setf goal (cons (instantiate (car goal) env)
  ;                   (cdr goal)))
  (let ((new-env (ask-top-level goal :env env)))
    (cond ((eq new-env :fail)
           :fail)
          (t
           (tell1 goal nil new-env)
           new-env))))

;;;ESTABLISH-ALL
(defun establish-all (goal &optional env)
  (when (member (car goal)
                '(exists forall not or))
    (error "Can't assert a pattern such as ~S" goal))
  ;(setf goal (cons (instantiate (car goal) env)
  ;                   (cdr goal)))
  (let ((new-envs (ask-top-level goal :all t :env env)))
    (cond ((eq new-envs :fail)
           :fail)
          (t
           (if (eq (car goal) 'and)
             (dolist (exp (cdr goal))
               (dolist (env new-envs)
                 (tell1 exp nil env)))
             (dolist (env new-envs)
               (tell1 goal nil env)))
           new-envs))))


;;;ASK-TOP-LEVEL
(defun ask-top-level (goal &key all env query-mode compiled)    
  (cond ((variable? goal)
         (multiple-value-bind (flag value)
                              (ocml-eval-var goal env)
           (cond (flag
                  (unless (listp value)
                    (error "A goal must be a list..when parsing ~s" value))
                  (ask-internal value all env query-mode compiled ))
                 (t
                  (error "An unbound variable such as ~A is not a legal goal"
                         value)))))
        (t (unless (listp goal)
             (error "A goal must be a list..when parsing ~s" goal))
           (ask-internal goal all env query-mode compiled ))))

;;;----------------------------------------------------

;;;SLOT-VALUES  ---Useful to get values of slots of domain instances (rather than using
;;;the more generic ask primitive)
(defun slot-values (name slot &optional parent-name)
  (cond (parent-name
         (let ((class (get-ocml-class parent-name)))
           (if class
             (let ((instance (find-current-direct-instance class name)))
               (if instance
                 (get-slot-values instance slot)
                 (error "~S is not a domain instance" name)))
             (error "~S is not a class" parent-name))))
        (t
         (Let ((instances (find-all-current-instances-named-x name)))
           (cond (instances
                  (if (cdr instances)
                    (error "Don't know which instance to use..many instances named ~S exist, ~s"
                           name instances)
                    (get-slot-values (car instances) slot)))
                 (t
                  (error "~S is not a domain instance" name)))))))





;;;THE-SLOT-VALUE
(defun the-slot-value (name slot)
  (let ((values (slot-values name slot)))
    (if values
      (car values)
      :nothing)))

(defun role-value (task role)
  (ocml-eval-gen `(role-value ,task ,role)))

(defun add-slot-value (name slot value &optional parent-name)
  (cond (parent-name
         (let ((class (get-ocml-class parent-name)))
           (if class
             (let ((instance (find-current-direct-instance class name)))
               (if instance
                 (maybe-add-slot-value-to-instance instance slot value)
                 (error "~S is not a domain instance" name)))
             (error "~S is not a class" parent-name))))
        (t
         (Let ((instances (find-all-current-instances-named-x name)))
           (cond (instances
                  (if (cdr instances)
                    (error "Don't know which instance to use..many instances named ~S exist, ~s"
                           name instances)
                    (maybe-add-slot-value-to-instance (car instances) slot value)))
                 (t
                  (error "~S is not a domain instance" name)))))))


(defun clear-slot-values (name slot &optional parent-name)
  (cond (parent-name
         (let ((class (get-ocml-class parent-name)))
           (if class
             (let ((instance (find-current-direct-instance class name)))
               (if instance
                 (remove-local-slot-values instance slot )
                 (error "~S is not a domain instance" name)))
             (error "~S is not a class" parent-name))))
        (t
         (Let ((instances (find-all-current-instances-named-x name)))
           (cond (instances
                  (if (cdr instances)
                    (error "Don't know which instance to use..many instances named ~S exist, ~s"
                           name instances)
                    (remove-local-slot-values (car instances) slot )))
                 (t
                  (error "~S is not a domain instance" name)))))))




(defun set-slot-value (name slot value &optional parent-name)
  (cond (parent-name
         (let ((class (get-ocml-class parent-name)))
           (if class
             (let ((instance (find-current-direct-instance class name)))
               (cond( instance
                      (remove-local-slot-values instance slot )
                      (add-slot-value-to-instance-int instance slot value))
                    (t
                     (error "~S is not a domain instance" name))))
             (error "~S is not a class" parent-name))))
        (t
         (Let ((instances (find-all-current-instances-named-x name)))
           (cond (instances
                  (if (cdr instances)
                    (error "Don't know which instance to use..many instances named ~S exist, ~s"
                           name instances)
                    (progn
                      (remove-local-slot-values (car instances) slot )
                      (add-slot-value-to-instance-int (car instances) slot value))))
                 (t
                  (error "~S is not a domain instance" name)))))))



;;;GET-ROLE-VALUE
(defun get-role-value (name slot)
  (ocml-eval-gen `(role-value ,name ,slot)))


;;;ACHIEVE-GENERIC-SUBTASK
(defun achieve-generic-subtask (supertask task-type &rest actual-role-pairs)
  (procedure-eval `(achieve-generic-subtask
                    ',supertask ',task-type
                    ,@(mapcar #'(lambda (x)
                                  (list 'quote x))
                              actual-role-pairs))))


;;;VALIDATE-CLASSES-IN-ONTOLOGY - Returns all the undefined classes used as 
;;;type constraints.  The input can be either the name of an ontology or 
;;;an ontology structure.  If empty, it defaults to the current ontology
(defun validate-classes-in-ontology (&optional (ontology *current-ontology*))
  (if (and (ontology? ontology)
           (known-ontology? ontology))
    (validate-classes-in-ontology-internal ontology)
    (let ((struct (get-ontology ontology)))
      (if struct
        (validate-classes-in-ontology-internal struct)
        (error "~s is not a known ontology"
               ontology)))))


(defun print-class-hierarchy (class-name &optional level (spaces 0) (stream t) (increment 3))
  ;;;level 0 prints only the root of the hierarchy
  (unless (numberp level)
    (setf level 100000000000))

  (spaces spaces) 
  (format  stream "~s~%" class-name)
  (let* ((class (get-ocml-class class-name))
         (direct-subclasses (direct-subclasses class))
         )
    (print-classes direct-subclasses (+ spaces increment) increment stream (- level 1))))



(defun print-classes (classes spaces increment stream level)
  (unless (< level 0)
    (loop for class in classes
          do
          (spaces spaces)
          (format  stream "~s~%" (name class))
          (print-classes (direct-subclasses class)
                         (+ spaces increment)
                         increment
                         stream
                         (- level 1)))))
          



