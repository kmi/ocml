;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defun do-class-definition (name superclasses instance-var  documentation    
				 class-slots relation-spec
                                 &aux supers lost-slots new-class slots-to-recompute domain-slots
                                  )
  (setf supers (mapcar #'get-domain-class superclasses))
  (when (member nil supers)
    (error "Some class in ~S has not been defined..when parsing class ~S" superclasses name))
  (setf supers  (standardise-superclasses supers)
        superclasses (mapcar #'name supers))
  (Let* ((local-slots-spec (parse-class-slots name class-slots))
         (local-slots (mapcar #'car local-slots-spec))
	;;;; (supers (mapcar #'get-domain-class superclasses))
         (inherited-slots (set-difference (class-inherited-slots supers)
                                          local-slots))
         (class (get-domain-class name))
	 (already-exists? class)
         (lisp-slots (init-arg-value :lisp-slots relation-spec))
         (internal-name (or (init-arg-value :lisp-class-name relation-spec)
                            (if already-exists?
                              (internal-name class)
                              (generate-internal-ocml-class-name name))))
         (renaming-pairs (second (member :slot-renaming relation-spec)))
         (ordered-supers))
    
    (when renaming-pairs 
      (setf renaming-pairs (check-local-renaming-feasibility 
                            name local-slots inherited-slots renaming-pairs)))
    (cond (already-exists?
           (warn "Redefining class ~S" name)
           (setf (find-class internal-name)  ;;Hope this works in all common lisp systems.....
                 class) 
           (setf lost-slots (set-difference (domain-slots class) 
                                            (append local-slots inherited-slots)))
           (remove-existing-slot-renaming-pairs class))
          ((and (find-class internal-name nil)
                (typep (find-class internal-name)'ocml-metaclass))
           ;;the ocml class does not exist, but the internal name is taken...
           (clear-subclasses-slot (find-class internal-name))
           (initialize-instances (find-class internal-name))
           (setf (home-ontology (find-class internal-name))
                 *current-ontology*)))
    (setf class (create-initial-class-definition 
                 internal-name superclasses local-slots lisp-slots)) 
     #+:FRANZ-INC 
     (setf (slot-value class 'clos:class-precedence-list)          ;;allegro needs a little kick......
           (clos:compute-class-precedence-list class))

     ;;the next lines have to do with slot renaming in classes --16/2/99
     
     (setf 
      (renaming-pairs class)  renaming-pairs
      (renaming-chains class) (construct-renaming-chains-for-class 
                               supers 
                               renaming-pairs)
     
      ;;let's save the original local slots spec before any renaming-related changes
      (ocml-options-no-renaming class) local-slots-spec)
      
      (setf
      ;;here we extend the local slots spec to include all renaming as well
       local-slots-spec (standardize-local-slots-spec-for-renaming  
                         local-slots-spec
                         (renaming-chains class))
      ;;finally, let's update local and inherited to reflect the above change
      local-slots (mapcar #'car local-slots-spec)
      inherited-slots (set-difference inherited-slots local-slots)
      ordered-supers (filter (cdr (class-precedence-list class))
				                       #'domain-class?))
     
    (multiple-value-bind (clos-specs slot-info-list ocml-options)
                         (finalize-class-spec  supers
                                               ordered-supers
                                               local-slots-spec
                                               inherited-slots)
      ;;here we ensure that all slots-info both local and 
      ;;inherited takes renaming into account
      (setf slot-info-list
            (standardize-slots-spec-for-renaming2  class ordered-supers slot-info-list ocml-options))
      
      (setf new-class (make-domain-class name internal-name supers instance-var documentation
			                 clos-specs
                                         (append local-slots inherited-slots)
                                         local-slots
                                         lisp-slots
                                         ocml-options
                                         slot-info-list
			                 relation-spec))
      (when already-exists?
        ;;;;;;;;;;;;;;;;;
        ;;;this line added because when renaming local and inherited slots are changed
        (setf lost-slots (set-difference lost-slots 
                                      (append local-slots inherited-slots)))
        ;;;;;;;;;;;;;;;;
        (dolist (slot lost-slots)
          (remove-slot-of-entry (get-relation slot) class))
        (Setf domain-slots (domain-slots new-class)
              slots-to-recompute                   ;;The slots to recompute are all the domain slots
              (union (domain-slots new-class)      ;;of the new class, + the slots 
		     lost-slots))                  ;;which have been 'lost'
       	(update-class-direct-instances new-class
                                       domain-slots);;Only domain slots are needed for instances 
	(update-subclasses new-class slots-to-recompute  lost-slots))
      ;;;harlequin can't quite cope
      #+lispworks
      (ensure-all-superclasses-know-me new-class supers)
      new-class)))

(defmethod redefine-subclass-after-class-update ((class ocml-metaclass)
                                                 slots-to-recompute
                                                 lost-slots) ;;;; new-supers lost-supers) 

  ;;slots-to-recompute = (domain-slots of redefined-superclass + lost-slots)
  ;;first we add to slots-to-recompute the slots which rename inherited slots. 
  ;; These renaming slots can derive either from local renaming pairs or from 
  ;;renaming chains inherited from other superclasses

  (with-slots (renaming-chains renaming-pairs slot-info-alist local-slots 
                               ocml-options-no-renaming ocml-options) 
              class
    (when renaming-chains
      (setf slots-to-recompute (add-local-renaming-chains 
                                slots-to-recompute renaming-chains)))
    
    (Let* ((superclasses (direct-domain-superclasses  class))
           (domain-slots-from-supers (class-inherited-slots superclasses))
           (domain-slots (union domain-slots-from-supers
                                (mapcar #'car ocml-options-no-renaming)))
           )
      (setf lost-slots (set-difference lost-slots domain-slots))
      ;;let's update renaming info
      (setf 
       renaming-pairs (remove-obsolete-renaming-pairs 
                       renaming-pairs domain-slots)
       renaming-chains (construct-renaming-chains-for-class
                        (direct-domain-superclasses class)
                        (renaming-pairs class)))
      
      (let*  ((relevant-local-slots) (other-relevant-slots) 
              (new-slot-info-list)
              (class-precedence-list (filter (cdr (class-precedence-list class))
                                             #'domain-class?)))
        (setf ocml-options
              ;;here we make sure that all local slot specifications
              ;;are consistent with renaming
              (standardize-local-slots-spec-for-renaming ocml-options-no-renaming
                                                         renaming-chains))
        
        ;;now, we need to update local-slots and related variables
        (setf local-slots (mapcar #'car ocml-options)
              relevant-local-slots (intersection local-slots slots-to-recompute)
              other-relevant-slots (intersection  ;;makes sure we do not deal with foreign slots
                                    (set-difference slots-to-recompute relevant-local-slots)
                                    domain-slots-from-supers))
        
        (loop for slot in relevant-local-slots
              for slot-info = (calculate-slot-info 
                               superclasses
                               class-precedence-list
                               slot
                               (right-value* slot ocml-options))
              do
              (push (cons slot slot-info) new-slot-info-list))
        
        (loop for slot in other-relevant-slots
              for slot-info = (calculate-slot-info 
                               superclasses
                               class-precedence-list
                               slot
                               nil)           ;;;Of course there are no local ocml options
              do
              (push (cons slot slot-info) new-slot-info-list))
        
        ;;now we add renaming-derived info to the slot-info-list 
        (setf new-slot-info-list
              (standardize-slots-spec-for-renaming2  class class-precedence-list new-slot-info-list
                                                     ocml-options t))
        
        ;;finally, let's makes sure that also teh slots we did not recompute are part 
        ;;of the new slot-info-alist
        (loop for pair in slot-info-alist
              do
              (unless (assoc (car pair) new-slot-info-list)
                (push pair new-slot-info-list)))
        
        
        (set-metaclass-information class domain-slots
                                   new-slot-info-list local-slots ocml-options)
        
        (when lost-slots
          (dolist (slot lost-slots) ;;;;;(set-difference lost-slots (domain-slots class)))
            (remove-slot-of-entry (get-relation slot) class)))
        (update-class-direct-instances class (append relevant-local-slots other-relevant-slots))))))


(defun standardize-slots-spec-for-renaming2 (class ordered-supers slot-info-list ocml-options
                                                   &optional partial-slot-info-list?)
    (loop with chains = (renaming-chains class)
          with result = slot-info-list
          for chain in chains
          do
          (setf result  (standardize-slots-spec-for-chain2 ordered-supers  result chain ocml-options
                                                          partial-slot-info-list?))
          finally
          (return result)))

(defun standardize-slots-spec-for-chain2 ( precedence-list slot-info-list chain ocml-options 
                                                           partial-slot-info-list? )
  (let* ((structures (mapcar #'(lambda (slot)
                                 (right-value slot slot-info-list))
                             chain))
         (result))
    (unless (and partial-slot-info-list?
                 (null (filter structures #'identity)))
      (setf result (merge-slot-info-structures2 precedence-list  chain  structures ocml-options))
      (loop for slot in chain
            do
            (setf slot-info-list
                  (substitute (cons slot result)
                              (assoc slot slot-info-list)
                              slot-info-list))))
    slot-info-list))
                                
   
(defun merge-slot-info-structures2 (precedence-list  chain  structures ocml-options)
  (let* ((min-cardinalities (filter (mapcar #'(lambda (structure)
                                                (get-slot-info-from-structure 
                                                 'min-cardinality
                                                 structure))
                                            structures)
                                    #'integerp))
         (max-cardinalities (filter (mapcar #'(lambda (structure)
                                                (get-slot-info-from-structure 
                                                 'max-cardinality
                                                 structure))
                                            structures)
                                    #'integerp))
         (max-cardinality (when max-cardinalities
                            (apply #'max  max-cardinalities)))
         (min-cardinality (when min-cardinalities
                            (apply #'max  max-cardinalities)))

        (values (remove-duplicates 
                 (mapcan* #'(lambda (structure)
                              (get-slot-info-from-structure 
                               'value
                               structure))
                          structures)
                 :test #'equal))
        (inheritance (multiple-value-bind (value flag)
                                          (find-first-option-value* precedence-list  chain 
                                                   :inheritance ocml-options)
                       (if value
                         (if flag
                           (car value)
                           value)
                         *default-inheritance*)))
                         
        (default-values (if (eq inheritance :supersede)
                          (find-first-option-value* precedence-list  chain :default-value ocml-options)
                          (remove-duplicates 
                           (mapcan* #'(lambda (structure)
                                        (get-slot-info-from-structure 
                                         'default-value
                                         structure))
                                    structures)
                           :test #'equal)))

        
        (types (remove-duplicates 
                         (mapcan* #'(lambda (structure)
                                      (get-slot-info-from-structure 
                                       'type
                                       structure))
                                  structures)
                         :test #'eq)))
        (create-slot-info-structure
         (List :inheritance inheritance
               :value values
               :default-value default-values
               :min-cardinality min-cardinality
               :max-cardinality max-cardinality
               :type types))))

(defun find-first-option-value* (precedence-list chain option ocml-options)
  ;;;each option (<slot> (option value) (option value))
  (let ((pair (assoc option 
                      (find option ocml-options
                            :test #'(lambda (x y)
                                      x
                                      (and (member (car y) chain)
                                           (assoc option y)))))))
    (if pair
      (values (cdr pair) :list)
      (loop for class in precedence-list
             for result1 = (loop for slot in chain
                                for result2 = (find-option-value class slot option)
                                until result2
                                finally (return result2))
            until result1
            finally (return result1)))))

