;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defun remove-ocml-class (name)
  (if (get-ocml-class name)
      (remove-ocml-class-aux name (get-ocml-class name))
      (error "~s is not an OCML class in the current ontology"name)))

(defun remove-ocml-class-aux (name class)
  (cond ((not (eq (home-ontology class) *current-ontology*))
         (ocml-warn "Class ~s has been defined in ontology ~s, which is different from the 
                         currently selected ontology, ~s.  Imported definitions cannot be deleted."
                    name (home-ontology class) *current-ontology*))
        ((subclasses  class)
         (ocml-warn "Class ~s cannot be deleted: subclasses exist for this class" 
                    name))
        (t
         (when (get-direct-instances class)
           (ocml-warn "Deleting class ~s existing class instances will be lost"
                    name))
         (remove-class-in-all-ontologies name class (get-relation name)))))


(defun can-remove-class? ( class)
  (not (subclasses class))) ;;;;;; (get-direct-instances class))))


;(defun remove-ocml-class-internal (name class relation)
;  (remove-domain-class name)
;  (remove-subclass class (direct-superclasses name class))
;  (when relation 
;    (remove-relation-internal name)))

;;redefined to handle changes across ontologies - Enrico 30-3-99
(defun remove-ocml-class-internal (name class relation)
  (remove-class-in-all-ontologies name class relation))

(defun remove-class-in-all-ontologies (name class relation)
  (remove-domain-class name)
  (with-slots (slots-as-relations )class
    (loop for relation in  slots-as-relations
          do
          (remove-slot-of-entry  relation class)
          (remove-local-slot-of-entry  relation class))) 
  (unassert-own-slots name class (own-slots class)) 
  (remove-subclass class (direct-superclasses name class))
  (maybe-fetch-definition-from-super-ontologies *current-ontology*
                                              'Ontology-classes
                                              name   *current-ontology*)
  (remove-def-from-dependent-ontologies name class 'class)
  (when (and relation 
             (defined-from-class? relation))
    (remove-relation-in-all-ontologies name relation)))

;;(defmethod can-remove-relation? ((relation ocml-relation))
;;  (with-slots (relation-instances  
;;               slot-of defined-by-rule fc-nodes)
;;              relation
;;    (not (or relation-instances
;;             slot-of  defined-by-rule fc-nodes))))




;;;Top level function to remove a relation
(defmethod remove-relation (name)
  (if (get-relation name)
      (remove-relation  (get-relation name))
      (error "~s is not an OCML relation in the current ontology"name)))

(defmethod remove-relation ((relation ocml-relation))
  (with-slots (name relation-instances upward-mapping? downward-add-exp downward-remove-exp 
                    slot-of own-slot-of defined-by-rule fc-nodes
                    home-ontology)
              relation
    (let ((class (get-ocml-class name)))
      (cond ((not (eq home-ontology *current-ontology*))
             (ocml-warn "Relation ~s has been defined in ontology ~s, which is different from the 
                         currently selected ontology, ~s.  Imported definitions cannot be deleted."
                        name home-ontology *current-ontology*))
            ((and class
                  (not (can-remove-class?  class)))
             (ocml-warn "Cannot delete relation ~s: associated class cannot be deleted"
                        name))
            (slot-of
             (ocml-warn 
              (string-append "Cannot delete relation ~s, which is a slot of classes ~s."
                             " These classes need to be deleted or modified first")
              name slot-of))
            ((cdr own-slot-of)
             (ocml-warn 
                    (string-append "Cannot delete relation ~s, which is a own slot of classes ~{~s, ~} ~s."
                             " These classes need to be deleted or modified first")
                    name (mapcar #'name (butlast own-slot-of))
                    (name (car (last own-slot-of)))))
            (own-slot-of 
             (ocml-warn 
                    (string-append "Cannot delete relation ~s, which is a own slot of class ~s."
                             " This class must  be deleted or modified first")
                    name (name (car own-slot-of))))
            (fc-nodes
             (ocml-warn "Cannot delete relation ~s: associated forward rules must be removed first"
                        name))
            (t
             (when relation-instances
               (ocml-warn "Deleting relation ~s: associated facts will be lost"
                          name))
             (when (or upward-mapping? downward-add-exp downward-remove-exp)
               (ocml-warn "Removing mapping information associated with relation ~s....."
                        name))
             (when defined-by-rule
               (ocml-warn "Removing rules information associated with relation ~s....."
                          name))
             (remove-relation-aux name class relation defined-by-rule))
             ))))

(defun remove-relation-aux (name class relation rules)
  (loop for rule in rules
        do
        (remove-bc-rule rule))
  (if class
    (remove-class-in-all-ontologies name class relation) ;;this also removes the relation
    (remove-relation-in-all-ontologies name relation)))

(defun remove-relation-in-all-ontologies (name relation)
  (remove-relation-internal name)
  (remove-argument-type-link-to-class name relation (argument-types relation))
  (maybe-fetch-definition-from-super-ontologies *current-ontology*
                                                'Ontology-relations 
                                                name   *current-ontology*)
  (remove-def-from-dependent-ontologies name relation 'relation))


           
  
  


  

