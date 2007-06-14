;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defclass ocml-method (task)
  ((applicable-to-task :initform nil :initarg applicable-to-task)))

(defun get-method (name)
  (get-task name))

(defun def-method-internal (name type documentation spec)
  (unless (stringp documentation)
    (Setf spec documentation
          documentation nil))
  (make-task name type documentation spec 'ocml-method))

(defmethod get-multi-valued-slots ((class (eql 'ocml-method)))
  '(applicable-to-task has-input-role has-control-role has-domain-requirement 
    modifies-role has-subtask))


(defclass executable-body (lisp-attachment-mixin documentation-mixin)
  ((free-vars-in-body :accessor free-vars-in-body)
   (body-expression :initarg :body-expression :reader body-expression )))



(defclass inference-body (executable-body) ())

(defclass control-body (executable-body) ())

(defmethod initialize-instance :after ((x executable-body) &rest initargs)
  (declare (ignore initargs))
  (set-free-vars-in-body x))


(defmethod set-free-vars-in-body  ((b  executable-body))
   (with-slots (body-expression free-vars-in-body) b
     (when body-expression
       (setf free-vars-in-body (collect-variables body-expression)))))


(defun def-control-body-internal (task-name documentation body &aux task)
  (unless (stringp documentation)
    (setf body documentation
          documentation nil))
  (setf task (get-task task-name))
  (if task
      (set-task-body task documentation body)
      (error "~s is not a task"task-name)))

(defun set-task-body (task documentation body &optional (type 'control-body))
  (setf (body task)
        (make-instance type
          :body-expression body
          :documentation documentation)))


(defun def-inference-body-internal (task-name documentation body &aux task)
  (unless (stringp documentation)
    (setf body documentation
          documentation nil))
  (setf task (get-task task-name))
  (if task
      (set-task-body task documentation body 'inference-body)
      (error "~s is not a task"task-name)))

(defmethod eval-body-expression  ((body control-body) env name output-role  )
  (declare (ignore  output-role))
  (with-slots (body-expression lisp-fun ) body
    (if lisp-fun
	(apply-lisp-fun name lisp-fun nil env )
	(task-level-eval body-expression env))))

(defmethod eval-body-expression ((body inference-body) env name output-role )
  (with-slots (body-expression lisp-fun ) body
    (set-role-value output-role
                    (if lisp-fun
                        (apply-lisp-fun name lisp-fun nil env )
                        (ocml-eval-fun body-expression env)))))

(defmethod eval-body-expression  :after ((body inference-body) env name output-role  )
  (declare (ignore env))
  (when *trace-tasks*
      (print-with-spaces *task-level* "~S returns ~S" 
                         name (get-role-value output-role))))



(defun apply-method-to-task2 (method-name task-name)
  (let (( task (get-task task-name)))
    (if task
      (set-task-body task nil method-name)
      (error "~s is not a task"task-name))))



