;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defclass axiom (name-mixin 
                  basic-ocml-object)
  ((expression :accessor axiom-expression :initarg :expression)))




(defun def-axiom-internal (name documentation axiom)
  (unless (stringp documentation)
    (setf axiom documentation
          documentation nil))
  #+(or allegro lispworks)(ocml-record-source-file name 'def-axiom)
  (new-axiom name documentation axiom)
  )

(defun new-axiom (name documentation  axiom)
  (let ((instance (make-instance 'axiom
                    :name name
                    :documentation documentation
                    :expression axiom))
        (old (get-axiom name )))
    (when old
      (warn "Redefining axiom ~S"name))
    (add-axiom-internal name instance)
    (propagate-new-def-to-sub-ontologies name instance 'axiom)
    instance))

(defun add-axiom-internal (name instance)
  (setf (gethash name *axioms*) instance))

(defun get-axiom (name)
  (gethash name *axioms*))

(defun get-axiom-expression (name)
  (when (get-axiom name)
    (axiom-expression (get-axiom name))))

(defun remove-axiom (name)
  (let ((axiom (get-axiom name)))
    (when axiom
      (cond ((not (eq (home-ontology axiom)
                      *current-ontology*))
             (ocml-warn "Axiom ~s has been defined in ontology ~s, which is different from the 
                         currently selected ontology, ~s.  Imported definitions cannot be renamed  
                         in a sub-ontology"
                        name (home-ontology axiom) *current-ontology*))
            (t
             (remove-axiom-in-all-ontologies name axiom))))))

(defun remove-axiom-in-all-ontologies (name axiom)
  (remove-axiom-internal name)
  (maybe-fetch-definition-from-super-ontologies *current-ontology*
                                              'Ontology-axioms 
                                              name   
                                              *current-ontology*)
  (remove-def-from-dependent-ontologies name axiom 'axiom))
  
    
(defun remove-axiom-internal (name)
  (remhash name *axioms*))
  

(defun clear-axioms ()
  (clrhash *axioms* ))


  