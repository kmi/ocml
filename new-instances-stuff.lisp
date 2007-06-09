;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defmethod add-instance-to-class ((class instances-meta)instance name)
  (with-slots (all-instances-table  direct-instances-list) class
    (push instance  direct-instances-list)
    (push instance (gethash name all-instances-table))
    (loop for super in (domain-superclasses class)
          do
          (add-instance-to-superclass-hash-table instance name super))))



(defun add-instance-to-superclass-hash-table (instance name class)
  (with-slots (all-instances-table ) class
    (push instance (gethash name all-instances-table))))


(defun remove-direct-instance-internal (name instance class)
  (with-slots (all-instances-table direct-instances-list) class 
    (Setf direct-instances-list (remove instance  direct-instances-list))
    (setf (gethash name all-instances-table)
          (remove instance (gethash name all-instances-table))))
    ;;;;;(remhash name instances))
  (loop for super in (domain-superclasses class)
          do
          (remove-instance-from-superclass-hash-table instance name super))
  (remove-all-instance-info-from-fc-rules instance (name class)))

(defun remove-instance-from-superclass-hash-table (instance name class)
  (with-slots (all-instances-table ) class
    (setf (gethash name all-instances-table)
          (remove instance (gethash name all-instances-table)))))


(defun find-current-direct-instance (class name)
  (with-slots (all-instances-table) class
    (find class (filter-current-instances (gethash name all-instances-table))
          :test #'(lambda (x y)
                    x
                    (eq (parent-class y)class)))))

(defun find-all-current-instances-of-class-named-x (x class-name)
  (let ((class (get-domain-class class-name)))
    (if class
      (with-slots (all-instances-table ) class
        (filter-current-instances (gethash x all-instances-table)))
      (error "Class ~s does not exist in the current ontology"
             class-name))))
                    

