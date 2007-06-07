;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defvar *current-task*)

;;;Need to define these ocml classes here, to be able to define the
;;;execute-task-instance method

(eval-when (eval load)
  (unless  (find-class  'task nil)
    (def-class TASK ()
      :lisp-class-name task))
  (unless (find-class  'executable-task nil)
    (def-class EXECUTABLE-TASK (task)
      :lisp-class-name executable-task))
  (unless (find-class 'problem-solving-method nil)
    (def-class problem-solving-method (executable-task)
      :lisp-class-name problem-solving-method)))


(defun EXECUTE-PRIMITIVE-TASK (body task-name)
  (let* ((output-role (the-slot-value task-name 'has-output-role))
         (value (if (symbolp body)
                  ;;it should be a procedure or function name
                  (ocml-call body task-name)
                  ;;It should be a unary lambda expression
                  (let* ((schema (second body))
                         (body (third body))
                         (free-vars (set-difference (collect-variables body)
                                                    schema)))
                    (multiple-value-bind (vars new-body)
                                         (rename-args-and-body 
                                          (append schema free-vars)
                                          body)
                      (procedure-eval new-body (list (cons (car vars) task-name))))))))
    (unless (eq output-role :nothing)
      (set-slot-value task-name output-role value))
    value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;The definition below are not used;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun EXECUTE-TASK (task-name)
  (let ((supertask (the-slot-value task-name 'subtask-of)))
    (when supertask
      (setf supertask (find-instance supertask 'task)))
     (execute-task-instance (find-instance task-name 'task)
                            supertask)))
    
;;;CALL-TASK
;;;Takes as arguments a task name and an optional list (role1 value1...).
;;;It sets the roles to the corresponding values and then executes the task
(defun call-task (class-name &rest args)        
  (let* ((supertask (when (boundp '*current-task*)      
                     *current-task*))
        (instance (define-domain-instance 
                    (gentemp class-name)
                    (append (when supertask
                              (list 'subtask-of (name supertask))))
                    (loop 
                          for i from 0 by 2
                          for j = (1+ i)
                          collect (List (elt args i) (elt args j))))))
    (execute-task-instance instance supertask)))

(defmethod EXECUTE-TASK-INSTANCE ((instance executable-task)super-instance)
  "This method executes a task by evaluating its body.
   before execution the method caches all the unbound role values 
   by inheriting them from supertasks.
   At the end the method sets the output role and returns the result"
  (let ((output-role (the-instance-slot-value instance 'has-output-role)))
    (when super-instance
      (cache-missing-role-values instance (name super-instance) output-role))
    (with-slots (instance-var name) instance
      (set-slot-value name output-role
                      (let ((*current-task* instance))
                        (procedure-eval (the-instance-slot-value instance 
                                                               'has-body)
                                        (list (cons instance-var name))))))))

(defmethod EXECUTE-TASK-INSTANCE :after ((method-instance problem-solving-method)
                                         super-instance)
  "Sets the output role of the associated task to the value returned 
   by the execution of the method" 
   super-instance ;ignore
  (let ((output-role (the-instance-slot-value method-instance 'has-output-role))
        (task (the-instance-slot-value method-instance 'tackles-task)))
    (set-slot-value task output-role
                    (the-instance-slot-value method-instance 
                                             'output-role))))



(defun cache-missing-role-values (instance super output-role)
  (let* ((input-roles (get-slot-values instance 'has-input-role))
         (name (name instance)))
    (loop 
          for role in (cons output-role input-roles)
          for lvalue = (or (get-local-slot-values instance  role)
                           :nothing)
          do
          (when (eq lvalue :nothing)
            (set-slot-value name 
                            role 
                            (ocml-eval-fun `(role-value ,super ,role)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;DEF-TASK-INTERNAL
(defun def-task-internal (name superclasses instance-var  documentation
                               class-slots relation-spec)
  (unless (or (eq name 'task)
              superclasses)
    (setf superclasses (list 'task)))
  (define-domain-class name superclasses instance-var  documentation
    class-slots relation-spec))

(defun apply-primitive-procedure (p args env &optional (eval-fun #'procedure-eval))
  (let ((result))
    (case p
    (do
      (dolist (form args)
        (setf result (funcall eval-fun form env)))
      result)
    (call (ocml-apply-internal  
                   (ocml-eval-fun (car args) env)
                   (new-evaluate-args  (cdr args) env eval-fun)
                   env
                   eval-fun))
    (procedure-eval (procedure-eval (procedure-eval (car args) env)env))
    ;;;;;;(procedure-eval (ocml-apply `(lambda (),(car args)) nil env eval-fun))
    (repeat
     (multiple-value-bind (actions test flag)
                          (parse-repeat-construct args)
       (repeat-actions actions test flag env eval-fun)))
    (if
      (evaluate-if-form args env eval-fun))
    (cond
     (evaluate-cond-form args env  eval-fun))
    (in-environment
     (evaluate-in-env-form args env eval-fun))
    (return 
     (throw 'ocml-control 
            (funcall eval-fun (car args) env)))
    (loop
      (evaluate-loop-form args env eval-fun))
    (tell (tell1 (maybe-evaluate-term (car args) env) nil
                   env))
    ;;;;(instantiate (cadr body)env) nil env))
    (unassert (unassert1 (maybe-evaluate-term (car args) env) env))
    ;;;(instantiate (cadr body)env)env))
    (t (error "Internal OCML Error")))))
  

(defun procedure-eval (body &optional env  (eval-fun #'procedure-eval))
  (cond ((and (consp body)
             (member (car body) *hardwired-procedures*))
           (apply-primitive-procedure (car body)(cdr body) env eval-fun))
          ((consp body)
           (Let ((f (get-function (car body))))
              (if f
                (if (procedure-p f)
                  (apply-ocml-f-or-p f (cdr body) env eval-fun)
                  (apply-ocml-f-or-p f (cdr body) env eval-fun))
	        (ocml-eval-fun-term (car body)         ;;Let's see whether is a fun expression
                                    (cdr body) env))))
          ((variable? body)
           (ocml-eval-var-or-error body env))
          (t
           body)))


(defun repeat-actions (actions test flag env eval-fun)
  (catch 'ocml-control
    (if flag
      (loop 
        until (eq :fail (ask-top-level test :env env))
        do 
        (loop for action in actions
	      do
	      (funcall eval-fun  action env)))
      (if test
        (loop 
          do 
          (loop for action in actions
	        do
	        (funcall eval-fun action env))
          until (eq (ask-top-level test :env env) :fail))
        (loop 
          do
          (loop for action in actions
	        do
	        (funcall eval-fun action env)))))))
        
      
      


(defun parse-repeat-construct (args)
  (if (= (length args)1)
    (values args nil nil)
    (if (member (car args) '(while until))
      (destructuring-bind (test &rest actions)
	                  (cdr args)
	(when (eq (car args) 'until)
	  (setf test (maybe-canonicalize-not `(not ,test))))
	(values actions test t))
      (let* ((l (length args))
             (keyword (elt args (- l 2)))
             test)
        (cond ((eq keyword 'while)
               (setf test (elt args (- l 1))))
              ((eq keyword 'until)
               (setf test (maybe-canonicalize-not `(not ,(elt args (- l 1))))))
              (t
               (error "can't find a WHILE or UNTIL clause...when parsing ~S"
                      args)))
        (values (subseq args 0 (- l 2))test nil)))))

