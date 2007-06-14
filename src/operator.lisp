;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package ocml)

;;;Operators can be used in the LHS of backward rules and in the RHS of forward rules to
;;;carry out operations such as adding new facts, printing, etc..
;;;In this file we define the machinery supporting the definition of operators

(defvar *operators* (make-hash-table))

(defun add-operator (name structure)
  (setf (gethash  name *operators*)structure))

(defun get-operator (name)
  (gethash  name *operators*))

(defun remove-operator (name)
  (remhash  name *operators*))

(defun clear-operators ()
  (clrhash *operators*))

(defclass ocml-operator (name-mixin lisp-attachment-mixin basic-ocml-object)
  ((arity :initarg :arity :initform nil :accessor arity)
   (schema :initarg :schema :initform nil)))

(defun make-ocml-operator (&rest options)
  (apply #'make-instance (cons 'ocml-operator options)))

      
;;;INITIALIZE-INSTANCE :AFTER OCML-OPERATOR
(defmethod initialize-instance :after ((op ocml-operator) &rest initargs)
  (declare (ignore initargs))
  (with-slots (name schema arity) op
    (enforce-arity-schema-consistency op name schema arity)
    (add-operator name op)))

(defmacro define-operator-internal (name schema documentation &rest options)
  (multiple-value-bind (name schema documentation options)
      (parse-define-operator-form name schema documentation options)
    `(funcall #'make-ocml-operator :name ',name :schema ',schema :documentation  ,documentation
	      ,@(mapcar #'(lambda (x)
                            (list 'quote x))
			options))))


(defun parse-define-operator-form (name schema documentation options)
  (parse-define-relation-form name schema documentation options 'operator))


(defmethod generate-candidates ((op ocml-operator) pred args)
  (declare (ignore pred args))
  (with-slots (lisp-fun) op
    lisp-fun))

(defun get-relation-or-operator (pred)
  (or (get-relation pred)
      (get-operator pred)))