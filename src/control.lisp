;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

;;;Need to define these ocml classes here, to be able to define the
;;;execute-task-instance method

(eval-when (:execute :load-toplevel :compile-toplevel)
  (unless  (find-class 'task nil)
    (def-class TASK ()
      :lisp-class-name task))
  (unless (find-class 'executable-task nil)
    (def-class EXECUTABLE-TASK (task)
      :lisp-class-name executable-task))
  (unless (find-class 'problem-solving-method nil)
    (def-class problem-solving-method (executable-task)
      :lisp-class-name problem-solving-method)))

(defun reset-trace-counter ()
  (setf *trace-depth-counter* 0))

(defun trace-this-task (task)
  (let ((type (task-class-or-instance? task)))
    (if type
      (pushnew task *traced-tasks*)
      (error  "~S is not a task class or instance" task))))

(defun tracing-this-task? (task-name)
  (or (eq *traced-tasks* :all)
      (and *traced-tasks*
           (or (member task-name *traced-tasks*)
               (let ((class (parent-class (find-current-instance task-name 'task))))
                 (or (member (name class) *traced-tasks*)
                     (intersection (mapcar #'name (domain-superclasses  class))
                                   *traced-tasks*)
                     (let ((task2 (findany '?x  `(tackles-task ,task-name ?x))))
                       (and (not (eq task2 :nothing))
                            (tracing-this-task? task2)))))))))
                          
                      

(defun untrace-this-task (task)
  (let ((type (task-class-or-instance? task)))
    (if type
      (setf  *traced-tasks* (remove task  *traced-tasks*))
      (warn  "~S is not a task class or instance" task))))



(defun task-class-or-instance? (name)
  (if (and (get-ocml-class name)
           (or (eq name 'task)
               (holds? 'subclass-of name 'task)))
    :class
    (when (holds? 'instance-of name 'task)
      :instance)))



;;;new from enrico 27/11/98
(defun input-roles (task-name)
  (setofall '?x `(has-input-role ,task-name ?x)))

(defun execute-primitive-task (name)
 
  (let ((body (the-slot-value name 'has-body)))
    (if (eq body :nothing)
         (if *in-irs*
             (remote-irs-procedure-call name (mapcar  #'(lambda (role)
                                                          (list role (role-value name role)))
                                                      (input-roles name)))
           (error "Can't execute ~a: no body found" name))
      (execute-task-body body name))))

(defun remote-irs-procedure-call (name pairs)
  name pairs
  (error "Oops..something went wrong.
This is not supposed to be executed - it is simply a place holder to avoid compiler warnings"))


;;;new from enrico 27/11/98

(defun EXECUTE-TASK-BODY (body task-name)
  (when (tracing-this-task? task-name)
    (incf *trace-depth-counter*)
    (print-with-spaces *trace-depth-counter*
                       "Enter task ~S with arguments ~{~s ~}"
                       task-name (mapcar #'(lambda (x)
                                             (list x (get-role-value
                                                      task-name x)))
                                             ;;;;;(the-slot-value task-name x)))
                                         (input-roles task-name))))

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
                      (procedure-eval new-body (list (cons (car vars)
task-name))))))))
    (unless (eq output-role :nothing)
      (set-slot-value task-name output-role value))

    (when (tracing-this-task? task-name)
      (print-with-spaces *trace-depth-counter*
                         "Exit task ~S -> ~S"
                         task-name value)
      (decf *trace-depth-counter*))

    value))


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
      (in-ontology
       (multiple-value-bind (fun body )
           (parse-in-ontology-construct args)
         (in-ontology-actions fun body  env eval-fun)))
     
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
      (unassert (unassert1 (maybe-evaluate-term (car args) env)
                           env))
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

(defun parse-in-ontology-construct (args)
  (if (= (length args)2)
    (values (car args) (second args))
    (error "Wrong number of arguments for IN-ONTOLOGY construct - expected 2 got ~s"
           (length args))))




(defun in-ontology-actions (fun body env eval-fun)
  (let ((ontology (funcall eval-fun fun env))
        (current-ontology *current-ontology*))
    (unwind-protect
      (progn
      (select-ontology ontology)
      (funcall eval-fun body env))
      (switch-to-ontology current-ontology))))



;;;FIND-ALL-PSMS-TACKLING-TASK-TYPE
;;;Takes the name of a task type and the name of an ontology in which the task is visible
;;;(but not necessarily its home ontology)
;;;and returns the list of all psms which can be used to 
;;;tackle the task.
;;;The output is a list of psm structures (i.e., class structures associated
;;;with psms)
;;;
;;;;;;;;;;;;;EXAMPLE;;;;;;;;;;;;;;;;;;;;
;;;
;;;(FIND-ALL-PSMS-TACKLING-TASK-TYPE 
;;; 'optimal-classification-task 
;;; 'classificationv2)
;;;
;;; returns
;;;
;;;(#<OCML-METACLASS HEURISTIC-OPTIMAL-SOL-CLASSIFIER0> 
;;; #<OCML-METACLASS NON-HIERARCHICAL-OPTIMAL-CLASSIFIER0>)
;;;

(defun find-all-psms-tackling-task-type (task-type 
                                         &optional (ontology
                                                   (name *current-ontology*)))
  (if (eq ontology (name *current-ontology*))
    (find-all-psms-tackling-task-type-internal task-type )
    (let ((current-ontology *current-ontology*))
    (unwind-protect 
      (progn
        (select-ontology ontology)
        (find-all-psms-tackling-task-type-internal task-type ))
      (switch-to-ontology current-ontology)))))

(defun find-all-psms-tackling-task-type-internal (task-type)
  (let* ((home-ontology (home-ontology (get-ocml-class task-type)))
         (relevant-ontologies (cons home-ontology
                                    (dependent-ontologies 
                                     home-ontology)))
         
         (result))
   (loop for ontology in (remove-subsumed-ontologies  relevant-ontologies)
          do
          (switch-to-ontology ontology)
          (setf result (append result 
                               (mapcar #'get-ocml-class
                                       (setofall '?x 
                                                 `(tackles-task-type 
                                                   ?x ,task-type))))))
      
    (remove-duplicates result)))


