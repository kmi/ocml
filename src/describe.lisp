;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


(defun all-ontology-instances (ontology)
  (let ((classes (ontology-classes (ontology-directory ontology)))
        (instances nil))
    (select-ontology (name ontology))
    (maphash
       #'(lambda (key value)
           (declare (ignore value))
           (when (get-domain-class key)
             (setf instances (append instances
                                     (all-current-direct-instances key)))))
       classes)
    (values (mapcar #'name instances) instances)))

(defun collect-names-matching-substring-in-all-ontologies (string 
                                                           &key 
                                                           (home-ontology-only? t)
                                                           (include-classes? t)
                                                           (include-relations? t)
                                                           (include-axioms? t)
                                                           (include-functions? t)
                                                           (include-bc-rules? t)
                                                           (include-instances? t))
  (Let ((result))
    (dolist (pair *all-ontologies*)
      (let ((result2 (collect-names-matching-substring-in-ontology 
             string (cdr pair)
             :home-ontology-only? home-ontology-only?
             :include-classes? include-classes? 
             :include-relations? include-relations?
             :include-axioms? include-axioms?
             :include-functions? include-functions?
             :include-bc-rules? include-bc-rules?
             :include-instances? include-instances?)))
        (when result2
          (push result2 result))))
    result))

   
(defun collect-names-matching-substring-in-ontology (string ontology &key 
                                                            (home-ontology-only?)
                                                            (include-classes? t)
                                                            (include-relations? t)
                                                            (include-axioms? t)
                                                            (include-functions? t)
                                                            (include-bc-rules? t)
                                                            (include-instances? t))
  (let* ((classes)
         (directory (ontology-directory ontology))
         (result (append 
                  (when include-classes?
                    (setf classes 
                          (collect-all-keys-containing-substring  
                           (ontology-classes directory)
                           ontology
                           string
                           home-ontology-only?))
                    (when classes
                      (list :classes classes)))
                  (when include-relations?
                    (Let ((keys
                           (collect-all-keys-containing-substring  
                            (ontology-relations directory) ontology
                            string
                            home-ontology-only? )))
                      (when classes
                        (setf keys (set-difference keys classes)))
                      (when keys 
                        (list :relations keys))))
                  (when include-functions?
                    (Let ((keys  (collect-all-keys-containing-substring  
                                  (ontology-functions directory)
                                  ontology
                                  string
                                  home-ontology-only?)))
                      (when keys
                        (list :functions keys))))
                  (when include-axioms?
                    (Let ((keys 
                           (collect-all-keys-containing-substring  
                            (ontology-axioms directory)
                            ontology
                            string
                            home-ontology-only?)))
                      (when keys
                        (list :axioms keys))))
                  (when include-bc-rules?
                    (Let ((keys 
                           
                           (collect-all-keys-containing-substring  
                            (ontology-bc-rules directory)
                            ontology
                            string
                            home-ontology-only?)))
                      (when keys 
                        (list :bc-rules keys))))
                  (when include-instances?
                    (multiple-value-bind (instance-keys instance-values)
                        (all-ontology-instances ontology)
                      (Let ((keys                            
                             (collect-all-keys-containing-substring-over-lists
                              instance-keys instance-values
                              ontology
                              string
                              home-ontology-only?)))
                        (when keys 
                          (list :instances keys))))))))
    (when result
      (cons result
            ontology))))
  
  
;(defun collect-all-keys-containing-substring (hash-table 
;                                                ontology 
;                                                string home-ontology-only? 
;                                                &aux result)
;    (maphash #'(lambda (key value)
;                 key
;                 (let ((key-string (format nil "~A" key )))
;                   (when (search string key-string :test #'string-equal)
;                     (unless (and home-ontology-only?
;                                  (not (eq (home-ontology value)ontology)))
;                       (push key result)))))
;             hash-table)
;    result)

(defun collect-all-keys-containing-substring (hash-table
                                                ontology
                                                string home-ontology-only?
                                                &aux result)
    (maphash #'(lambda (key value)
                 key
                 (let ((key-string (if (and (atom key) (symbolp key))
                                       (symbol-name key)
                                       (format nil "~A" key ))))
                   (when (search string key-string :test #'string-equal)
                     (unless (and home-ontology-only?
                                  (not (eq (home-ontology value)ontology)))
                       (push key result)))))
             hash-table)
    result)
  
  
(defun collect-all-keys-containing-substring-over-lists (keys values
                                                              ontology
                                                              string home-ontology-only?
                                                              &aux result)
  
  (mapc #'(lambda (key value)
           
              (let ((key-string (if (and (atom key) (symbolp key))
                                  (symbol-name key)
                                  (format nil "~A" key ))))
                (when (search string key-string :test #'string-equal)
                  (unless (and home-ontology-only?
                               (not (eq (ocml::home-ontology value)
                                        ontology)))
                    
                    (push key result)))))
        keys values)
  result)


(defun find-slot-documentation (class slot)
  (flet ((get-doc (class1 slot1)
           (right-value* :documentation 
                        (cdr (assoc slot1 (ocml-options class1))))))
    (or (get-doc class slot)
        (loop 
          for super in (filter (class-precedence-list class)
                                   #'domain-class?)
              for doc = (get-doc super slot)
              until doc
              finally (return doc)))))
              
 

(defun describe-instance (name &key class-name deduce-all-values?)
  "If deduce-all-values? is T then we use all inference methods
   at our disposal to find slot values"
  (cond (class-name
         (Let ((insts (find-all-current-instances-of-class-named-x name class-name)))
           (cond (insts
                  (if (cdr insts)
                    (let ((inst (find class-name insts
                                      :test #'(lambda (x y)
                                                x
                                                (eq (name (parent-class y))
                                                    class-name)))))
                      (cond (inst
                             (describe-instance-internal inst deduce-all-values?))
                            (t
                             (warn "several instances named ~s of class ~s exist, ~s..choosing one randomly to describe..."
                                   name class-name insts)
                             (describe-instance-internal (car insts) deduce-all-values?))))
                    (describe-instance-internal (car insts) deduce-all-values?)))
                 (t
                  (format t "No instance of class ~s named ~s can be found"
                          class-name name)))))
        (t
         (Let ((insts (find-all-current-instances-named-x name )))
           (cond (insts
                  (cond ((cdr insts)
                         (warn "several instances named ~s exist, ~s...choosing one randomly to describe..."
                               name insts)
                         (describe-instance-internal (car insts) deduce-all-values?))
                        (t
                         (describe-instance-internal (car insts) deduce-all-values?))))
                 (t
                  (format t "No instance named ~s can be found"
                          name)))))))
                    
                             
            
(defun describe-instance-internal (inst deduce-all-values?)            
  (let ((unbound-slots)
        (parent)
        (name (name inst)))
    (setf parent (parent-class inst))
    (format t "~%Instance ~s of class ~S~%"
            name (name parent))
    (format t "~%Home Ontology: ~S~%"
            (name (home-ontology inst)))
    (loop for slot in (domain-slots parent)
          for values = (if deduce-all-values?
                         (setofall '?x `(,slot ,name ?x))
                         (get-slot-values inst slot))
          ;;;;;when values
          do
          (if values
            (format t "~%~S: ~S~{, ~S~}~%"
                    slot (car values)(cdr values))
            (push slot unbound-slots))
          finally
          (when unbound-slots
            (format t "~%The following slots have no value: ~S~{, ~S~}~%"
                    (car unbound-slots)(cdr unbound-slots))))
    
    (values)))


(defun describe-class (name &optional local-defs-only?)
  (if local-defs-only?
    (describe-class-local-info name)
    (describe-class-info name)))


;;;DESCRIBE-CLASS-INFO -  Prints out all the currently applicable
;;;information about a class.  By 'currently applicable' I mean
;;;that superseded slot values are not displayed.
;(defun describe-class-info (name)
;  (Let* ((class (get-ocml-class name))
;         (supers (mapcar #'name (domain-superclasses class))))
;    (when class
;      (format t "~%Class ~S~%" name)
;      (format t "~% Superclasses: ~S~{, ~S~}"
;              (car supers)
;              (cdr supers))
;      (loop for slot in (domain-slots class)
;            do 
;            (format t "~2% ~s"slot)
;            (multiple-value-bind (values defaults)
;                                 (get-slot-values-from-class-structure 
;                                  class slot )
;              (if values
;                (format t "~%  ~S: ~S~{, ~S~}"
;                          :value
;                          (car values)
;                          (cdr values))
;                (when defaults
;                  (format t "~%  ~S: ~S~{, ~S~}"
;                          :default-value
;                          (car defaults)
;                          (cdr defaults)))))
;            (let ((type-info (find-option-value class slot :type)))
;              (when type-info
;                (format t "~%  ~S: ~S~{, ~S~}"
;                          :type
;                          (car type-info)
;                          (cdr type-info))))
;            (loop for option in '(:min-cardinality 
;                                  :max-cardinality
;                                  :inheritance)
;                  for value = (find-option-value class slot option)
;                  when value
;                  do
;                  (format t "~%  ~S: ~S"
;                          option value))
;            (let ((doc (find-slot-documentation class slot)))
;              (when doc
;                (format t "~%  ~S: ~S"
;                          :documentation doc)))))))
;
;(defun describe-class-local-info (name)
;    
;  (Let* ((class (get-ocml-class name))
;         (supers (mapcar #'name (domain-superclasses class))))
;    (when class
;      
;      (format t "~%Class ~S~%" name)
;      (format t "~% Superclasses: ~S~{, ~S~}"
;              (car supers)
;              (cdr supers))
;      
;      (loop 
;            for slot-info in (ocml-options class)
;            do 
;            (format t "~2% ~s"(car slot-info))
;            (loop 
;                  for pair in (cdr slot-info)
;                  do
;                  (format t "~%  ~S: ~S"
;                          (car pair)
;(second pair)))))))


;;;DESCRIBE-CLASS-INFO -  Prints out all the currently applicable
;;;information about a class.  By 'currently applicable' I mean
;;;that superseded slot values are not displayed.
(defun describe-class-info (name)
  (Let* ((class (get-ocml-class name))
         (supers (mapcar #'name (domain-superclasses class)))
         (subs (mapcar #'name (current-subclasses class))))
    (when class
      (format t "~%Class ~S~%" name)
      (format t "~% Ontology: ~s~%" (name (home-ontology class)))
      
      (format t "~% Superclasses: ~S~{, ~S~}"
              (car supers)
              (cdr supers))
      (format t "~2% Subclasses: ~S~{, ~S~}"
              (car subs)
              (cdr subs))
      (loop with chains = (renaming-chains class)
            for slot in (domain-slots class)
            do
            (unless (slot-renamed? slot chains)
              (format t "~2% ~s"slot)
              (multiple-value-bind (values defaults)
                                   (get-slot-values-from-class-structure
                                    class slot )
                (if values
                  (format t "~%  ~S: ~S~{, ~S~}"
                          :value
                          (car values)
                          (cdr values))
                  (when defaults
                    (format t "~%  ~S: ~S~{, ~S~}"
                            :default-value
                            (car defaults)
                            (cdr defaults)))))
              (let ((type-info (remove-duplicates 
                                (find-option-value class slot :type))))
                (when type-info
                  (format t "~%  ~S: ~S~{, ~S~}"
                          :type
                          (car type-info)
                          (cdr type-info))))
              (loop for option in '(:min-cardinality
                                    :max-cardinality
                                    :inheritance)
                    for value = (find-option-value class slot option)
                    when value
                    do
                    (format t "~%  ~S: ~S"
                            option value))
              (let ((doc (find-slot-documentation class slot)))
                (when doc
                  (format t "~%  ~S: ~S"
                          :documentation doc))))
            finally
            (when chains
              (format t "~2%The following renaming chains apply: ~2%~{ ~s ~%~}~%"
                      chains))))))
              
(defun describe-class-local-info (name)

  (Let* ((class (get-ocml-class name))
         (supers (mapcar #'name (domain-superclasses class)))
         (subs (mapcar #'name (current-subclasses class))))
    (when class

      (format t "~%Class ~S~%" name)
      (format t "~% Ontology: ~s~%" (name (home-ontology class)))
      (format t "~% Superclasses: ~S~{, ~S~}"
              (car supers)
              (cdr supers))
      (format t "~2% Subclasses: ~S~{, ~S~}"
              (car subs)
              (cdr subs))

      (loop
            for slot-info in (ocml-options class)
            do
            (format t "~2% ~s"(car slot-info))
            (loop
                  for pair in (cdr slot-info)
                  do
                  (format t "~%  ~S: ~S"
                          (car pair)
                          (second pair)))))))

(defun describe-relation (name)
  (Let* ((rel (get-relation name)))
    (if rel
      (describe-relation-internal name rel)
      (ocml-warn "~s is not a relation" name))))


#|
(defun describe-relation-internal (name rel)
  (let ((flag t))
    (format t "~%Relation ~S ~s~%" name (schema rel))
    (format t "~% Ontology: ~s~%" (name (home-ontology rel)))
    (if (ocml-documentation rel)
      (format t "~% Documentation: ~s~%" (ocml-documentation rel))
      (setf flag nil))
    
    (when (local-slot-of rel)
      (unless flag
        (format t "~%"))
      (format t "~% Local slot of: ~S~{, ~S~}"
              (name (car (local-slot-of rel)))
              (mapcar #'name  (cdr (local-slot-of rel)))))
    (when (slot-of rel)
      (Let ((non-local-slots (set-difference (slot-of rel)(local-slot-of rel))))
        (when non-local-slots
          (format t "~2% Also slot of: ~S~{, ~S~}"
                  (name (car non-local-slots))
                  (mapcar #'name (cdr non-local-slots))))))
    (unless flag
      (format t "~%"))
    (when (constraint rel)
      (format t "~% Constraints: ~s~%" (constraint rel)))
    
    (when (iff-def rel)
      (format t "~% Iff-def: ~s~%" 
              (car (bc-clause-antecedents  (iff-def rel)))))
    
    (when (sufficient rel)
      (format t "~% Sufficient: ~s~%"    
              (car (bc-clause-antecedents (sufficient rel)))))
    (when (defined-by-rule rel)
      (format t "~% Associated rules: ~S~{, ~S~}~%"
                  (name (car (defined-by-rule rel)))
                  (mapcar #'name
                          (cdr (defined-by-rule rel)))))
                           
    (when (prove-by rel)
      (format t "~% Prove by: ~s~%"  (car (bc-clause-antecedents
                                           (prove-by rel)))))
    
    (when (lisp-fun rel)
      (format t "~% Prove by: ~s~%" (lisp-fun rel)))
    
    (when (relation-instances rel)
      (format t "~% Relation Instances: ~S~{, ~S~}"
             (args (car (relation-instances rel)))
              (mapcar #'args (cdr (relation-instances rel)))))))
|#

;;
(defun describe-relation-internal (name rel)
  (let ((flag t))
    (format t "~%Relation ~S ~s~%" name (schema rel))
    (format t "~% Ontology: ~s~%" (name (home-ontology rel)))
    (if (ocml-documentation rel)
      (format t "~% Documentation: ~s~%" (ocml-documentation rel))
      (setf flag nil))
   
    (when (local-slot-of rel)
      (unless flag
        (format t "~%"))
      (format t "~% Local slot of: ~S~{, ~S~}"
              (name (car (local-slot-of rel)))
              (mapcar #'name  (cdr (local-slot-of rel)))))
    (when (slot-of rel)
      (Let ((non-local-slots (set-difference (slot-of rel)(local-slot-of rel))))
        (when non-local-slots
          (format t "~2% Also slot of: ~S~{, ~S~}"
                  (name (car non-local-slots))
                  (mapcar #'name (cdr non-local-slots))))))
    (unless flag
      (format t "~%"))
    (when (constraint rel)
      (format t "~% Constraints: ~s~%" (constraint rel)))
   
    (when (iff-def rel)
      (format t "~% Iff-def: ~s~%"
              (if (bc-clause? (iff-def rel))
                (car (bc-clause-antecedents (iff-def rel)))
                (iff-def rel))
              ))
   
    (when (sufficient rel)
      (format t "~% Sufficient: ~s~%" 
              (if (bc-clause? (sufficient rel))
                (car (bc-clause-antecedents (sufficient rel)))
                (sufficient rel))))
    (when (defined-by-rule rel)
      (format t "~% Associated rules: ~S~{, ~S~}~%"
              (name (car (defined-by-rule rel)))
              (mapcar #'name
                      (cdr (defined-by-rule rel)))))
   
    (when (prove-by rel)
      (format t "~% Prove by: ~s~%"  (car (bc-clause-antecedents
                                           (prove-by rel)))))
   
    (when (lisp-fun rel)
      (format t "~% Prove by: ~s~%" (lisp-fun rel)))
   
    (when (relation-instances rel)
      (format t "~% Relation Instances: ~S~{, ~S~}"
              (args (car (relation-instances rel)))
              (mapcar #'args (cdr (relation-instances rel)))))))

(defun describe-function (name)
  (Let* ((f (get-function name)))
    (if f
      (describe-function-internal name f)
      (ocml-warn "~s is not a function" name))))

(defun describe-function-internal (name f)
  (let ((flag t))
    (format t "~%Function ~S ~s~%" name (schema f))
    
    (format t "~% Ontology: ~s~%" (name (home-ontology f)))
    (if (ocml-documentation f)
      (format t "~% Documentation: ~s~%" (ocml-documentation f))
      (setf flag nil))
    
    (unless flag
      (format t "~%"))
    (when (constraint f)
      (format t "~% Constraints: ~s~%" (constraint f)))
    
    (when (definition f)
      (format t "~% Def: ~s~%" 
              (definition f)))
    
    (when (body f )
      (format t "~% Body: ~s~%"    
              (body f)))
    
    (when (lisp-fun f)
      (format t "~% Lisp attachment: ~s~%" (lisp-fun f)))
    
    ))



  
  
  
