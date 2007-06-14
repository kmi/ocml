;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


;;;;to check change-instance-parent-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-relation-instance-constraints (instance relation)
  (loop 
    with violations = nil
    for constr in (fetch-constraints-from-relation relation)
    for flag = (check-constraint constr (cons (name relation) (args instance )))
    do
    (unless flag
      (push constr violations))
    finally
    (when violations
      (ocml-warn "Relation instance ~s violates the following applicable constraints: ~%~{ ~s~%~}"
                 (cons (name relation) (args instance ))  violations))))


(defun check-slot-assertion-constraints (name slot values)
  (loop 
    with violations = nil
    for constr in (fetch-constraints-from-relation (get-relation slot))
    do
    (loop for value in values
          for flag = (check-constraint constr (list slot name value))
          do
          (unless flag
            (push (List value (transform-slot-constraint slot constr)) violations)))
    finally
    (when violations
      (ocml-warn "The following values of slot ~s of instance ~s violate the following applicable constraints:
~:{ value ~s violates constraint ~a~%~}"
                 slot name violations))))

(defun check-instance-constraints (instance parent)
  (loop 
    with violations = nil
    for constr in (fetch-applicable-constraints parent)
    for flag = (check-constraint constr (name instance))
    do
    (unless flag
      (push (list constr (find-constraint-in-ancestor constr instance))
            violations))
    finally
    (when violations
      (ocml-warn "Instance ~s of class ~s violates the following constraints:
~:{ ~s inherited from class ~s~%~}"
               (name instance) (name parent) violations))))

;;;CHECK-INSTANCE-SLOT-CONSTRAINTS
;;;This function is called after an instance is created or an ancestor class is redefined
;;;It checks that all slot values in all domain slots satisfy the applicable constraints
;;;
(defun check-instance-slot-constraints (instance)
  (loop for slot in (domain-slots instance)
        do
        (check-slot-assertion-constraints (name instance) slot (get-slot-values instance slot))))


(defun transform-slot-constraint (slot constr)
  (destructuring-bind (kappa schema body) constr
    (list kappa schema
          (list '=> (cons slot schema) body))))  

(defun check-constraint (constraint &rest args)
  (apply #'holds? constraint args))

(defun fetch-applicable-constraints (class)
  (let ((supers (domain-superclasses class)))
    (mapcan* #'(lambda (c)
                (fetch-constraints-from-class (name c)))
            (cons class supers))))

(defun fetch-constraints-from-class (class-name)
  (fetch-constraints-from-relation (get-relation class-name)))

(defun fetch-constraints-from-relation (rel)
  (Let ((constraint (constraint rel))
        (iff-def (iff-def rel)))
    (when constraint
      (setf constraint `(kappa ,(schema rel) ,constraint)))
     (when iff-def
      (setf iff-def `(kappa ,(schema rel) ,(first (bc-clause-antecedents iff-def)))))
     (remove-if #'null (list constraint iff-def))))

(defun find-constraint-in-ancestor (constraint instance)
  (let* ((parent (parent-class instance))
         (supers (domain-superclasses parent)))
    (loop for c in (cons parent supers)
          for constraints = (fetch-constraints-from-class (name c))
          do
          (when (member constraint constraints :test #'equal)
            (return (name c))))))


  
    

    