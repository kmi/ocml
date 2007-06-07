;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


;;;Need to define these ocml classes here, to be able to define the
;;;execute-task-instance method
(def-class TASK ()
  :lisp-class-name task)

(def-class EXECUTABLE-TASK (task)
  :lisp-class-name executable-task)

;;;CALL-TASK
;;;Takes as arguments a task name and an optional list (role1 value1...).
;;;It sets the roles to teh corresponding values and then executes the task
(defun call-task (name &rest args)
  (let ((instance (find-instance name 'task)))
    (cond (instance
           (if args
             (loop with previous-values
                   for i from 0 by 2
                   for j = (1+ i)
                   for slot = (elt args i)
                   for value = (or (get-local-slot-values instance  slot)
                                   :nothing)
                   do
	           (push (cons slot value) previous-values)
	           (set-slot-value name slot (elt args j))
                   until (= j (1- (length args)))
                   finally
                   (return 
                    (prog1
	              (execute-task-instance instance)
                     (dolist (pair previous-values)
                       (if (eq (cdr pair) :nothing)
                         (remove-local-slot-values instance (car pair))
		         (set-slot-value name (car pair) (cdr pair)))))))
             (execute-task-instance instance)))
          (t
           (error "~s is not a task" name)))))

(defmethod execute-task-instance (executable-task)
  "This method executes a task by evaluating its body.
   before execution the method caches all the unbound role values 
   by inheriting them from supertasks.
   At the end the method sets the output role and returns the result"
  (with-slots (instance-var name) task-instance
    (let* ((input-roles (get-slot-values task-instance 'has-input-role))
           (output-role (the-instance-slot-value task-instance 'has-output-role))
           (result  (loop with  unbound-roles
                          for role in (cons output-role input-roles)
                          for lvalue = (or (get-local-slot-values instance  role)
                                           :nothing)
                          do
                          (when (eq lvalue :nothing)
                            (push role
                                  unbound-roles)
                            (set-slot-value name role 
                                            (ocml-eval-fun `(role-value ,name 
                                                                        ,role))))
                          finally
                          (return
                           (prog1 
                             (procedure-eval (the-instance-slot-value task-instance 
                                                                      'has-body)
                                             (list (cons instance-var name)))
                             (dolist (role unbound-roles)
                               (remove-local-slot-values instance role)))))))
      (set-slot-value name output-role result)
      result)))
      
  
  
;;;DEF-TASK-INTERNAL
(defun def-task-internal (name superclasses instance-var  documentation
                               class-slots relation-spec)
  (unless (or (eq name 'task)
              superclasses)
    (setf superclasses (list 'task)))
  (define-domain-class name superclasses instance-var  documentation
    class-slots relation-spec))


(defun procedure-eval (body env &optional (eval-fun #'procedure-eval))
  (cond ((consp body)
         (case (car body)
      (do-actions
       (dolist (task (cdr body))
         (funcall eval-fun task env)))
      (repeat-actions
       (multiple-value-bind (actions test flag)
	   (parse-repeat-construct (cdr body))
         (repeat-actions actions test flag env eval-fun)))
      (if
	  (evaluate-if-form (cdr body) env eval-fun))
      (cond
       (evaluate-cond-form (cdr body) env  eval-fun))
      (in-environment
       (evaluate-in-env-form (cdr body) env eval-fun))
      (loop
       (evaluate-loop-form (cdr body) env eval-fun))
      (tell (tell1 (instantiate (cadr body)env) nil env))
      (unassert (unassert1 (instantiate (cadr body)env)env))
      (t
       (Let ((f (get-function (car body))))
         (if f
             (if (procedure-p f)
                 (apply-ocml-f-or-p f (cdr body) env eval-fun)
                 (apply-ocml-f-or-p f (cdr body) env #'ocml-eval-fun))
	     (ocml-eval-fun-term (car body)         ;;Let's see whether is a fun expression
                                 (cdr body) env))))))
        ((variable? body)
         (ocml-eval-var-or-error body env))
        (t
         body)))


(defun repeat-actions (actions test flag env eval-fun)
  (if flag
      (loop 
        until (eq :fail (ask-top-level test :env env))
        do 
        (loop for action in actions
	      do
	      (funcall eval-fun  action env)))
      (loop 
       do 
        (loop for action in actions
	      do
	      (funcall eval-fun action env))
	 until (eq (ask-top-level test :env env) :fail))))


(defun parse-repeat-construct (args)
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
        (values (subseq args 0 (- l 2))test nil))))

