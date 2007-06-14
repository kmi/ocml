;;; Mode: Lisp; Package: ocml

;;; Author: John Domingue

;;; The Open University

(in-package "OCML")

(in-ontology base-ontology)

(defun most-common-elements (x)
  (setf x (remove nil x))
  (unless (null x)
    (let ((winners (list (car x)))
          (winner-count (count (car x) x)))
      (dolist (y (cdr x))
        (let ((y-count (count y x)))
          (cond ((> y-count winner-count)
                 (setf winners (list y)
                       winner-count y-count))
                ((= y-count winner-count)
                 (pushnew y winners)))))
      (values winners winner-count))))

(defun intersection* (&rest lists)
  (cond ((null lists) nil)
        ((= (length lists) 1) (car lists))
        (t (apply #'intersection* (cons (intersection (car lists) (second lists))
                                        (cddr lists))))))

(defun common-ancestor (objects top-class)
  (let ((result
         (if (= (length objects) 1)
             (car objects)
             (car (apply #'intersection* (mapcar #'instance-superclasses objects))))))
    (unless (eq result top-class)
      result)))

(defun instance-superclasses (instance)
  (ocml-eval-gen `(setofall ?x (instance-of ,instance ?x))))
                       
(def-function most-common-elements (?list)
  :lisp-fun
  #'(lambda (x)
      (most-common-elements x)))

(def-function common-ancestor (?list ?top-class)
  :lisp-fun
  #'(lambda (x y)
      (common-ancestor x y)))

