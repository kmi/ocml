;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


;;;*************************************************************
;;;;;;;;;;;;;;;;;;DOMAIN INSTANCES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun domain-instance? (thing)
  (typep thing 'basic-domain-class))

;;; NEW-INSTANCE¬- returns a new instance of a class with the given attributes
;;;returns an instance structure
(defun new-instance (type &optional pairs)
    (define-domain-instance (GENTEMP "INSTANCE") type ""
                             pairs))


(defun define-domain-instance (name parent &optional documentation slots)
  (let ((class (get-domain-class parent)))
    (unless class
      (error "Class ~s has not been defined.....when evaluating (DEF-INSTANCE ~S....)"
             parent name))
    (unless (stringp documentation)
      (setf slots documentation)
      (setf documentation nil))
    (new-domain-instance-internal name parent class documentation slots)))

(defun new-domain-instance-internal (name parent class documentation slots)
    (setf slots (mapcan #'(lambda (slot-spec)
                            (list (car slot-spec)
                                  (mapcar #'eval-functional-arg
                                          (cdr slot-spec))))
                        slots))
    (let ((old-instance (find-current-direct-instance class  name))
	  (real-parent (internal-name class))
          instance)

      (setf instance
            (apply #'make-instance
                   `(,real-parent :name ,name ,@slots :documentation ,documentation)))
      (when old-instance
        (when (tracing-this-assertion? (List parent name))
              (print-with-spaces (1+ *task-level*)
                                 "Redefining instance ~s of class ~s"
                                 name parent))
        (let ((*traced-assertions* nil))
          (if (eq (home-ontology old-instance)
                    *current-ontology*)
            ;;if instance with same name in different ontology we do not
            ;;need to remove it
            (remove-direct-instance old-instance name)
            (pushnew instance (instance-overridden-by old-instance)))))

      #-:lispworks(record-source-file name 'ocml-class-instance)
      ;;#+(or allegro lispworks)(record-source-file name 'def-instance)
      #+(or allegro lispworks)(ocml-record-instance-source-file name parent
                                                                'def-instance)
      (cache-inherited-slots instance class)
      (tell-fc-new-instance instance parent)
      (when *check-constraints*
        (check-instance-constraints instance class)
        (check-instance-slot-constraints instance))
      instance))


(defun change-instance-parent-class (instance-name old-parent-name new-parent-name)
  (let* ((old-parent (get-ocml-class old-parent-name))
         (Instance (find-current-direct-instance old-parent instance-name))
         (new-parent (get-ocml-class new-parent-name)))
    (cond ((and instance old-parent new-parent)
           (let* ((local-slots (filter (domain-slots instance)
                                      #'(lambda (slot)
                                          (get-local-slot-values instance slot))))
                 (new-domain-slots (domain-slots new-parent))
                 (lost-slots (set-difference local-slots new-domain-slots))
                 (new-local-slots (set-difference local-slots lost-slots)))
             (when lost-slots
               (ocml-warn "Local values for slots ~{~s ~} in instance ~s are being lost...."
                          lost-slots instance-name))
             (let ((spec (mapcar #'(lambda (slot)
                                     (cons slot (get-local-slot-values instance slot)))
                                 new-local-slots)))
               (remove-direct-instance instance instance-name old-parent-name old-parent)
               (new-domain-instance-internal instance-name new-parent-name new-parent 
                                             (ocml-documentation instance)
                                             spec))))
          (t
           (unless instance
             (error "cannot find instance named ~s" instance-name))
           (unless old-parent 
             (error "cannot find class named ~s" old-parent))
           (unless new-parent 
             (error "cannot find class named ~s" new-parent))))))
           

(defun rename-instance (old-name parent-class new-name)
  (let ((instance (find-current-direct-instance parent-class old-name)))
    (cond (instance 
           (remove-direct-instance instance old-name)
           (setf (name instance) new-name)
           (add-instance-to-class (parent-class instance) instance new-name)
           (tell-fc-new-instance instance (name (parent-class instance))))           
          (t
           (error "~s is not an instance" old-name)))))
  


;;;MAYBE-REMOVE-DOMAIN-INSTANCE -- Kills an instance named <name> if it is an instance
;;; of <parent>.
;;(defun maybe-remove-domain-instance (name class)
;;  (let ((instance (find-current-instance-of-class class  name)))
;;    (when instance
;;      (remove-direct-instance instance name ))))


;;;REMOVE-DOMAIN-INSTANCE-GEN ---Removes all instances of class, which match
;;;inst-term.
;;(defun remove-domain-instance-gen (inst-term parent class)
;;  (if (variable? inst-term)
;;      (remove-all-current-instances class parent)                  
;;      (maybe-remove-domain-instance inst-term class)))


(defun remove-domain-instance-gen (inst-term parent class)
  (if (variable? inst-term)
      (remove-all-current-instances class parent)                  
      (remove-all-current-instances-named-x-of-class inst-term class)))

(defun remove-all-current-instances-named-x-of-class (x class)
  (loop for instance in (find-all-current-instances-of-class-structure-named-x 
                    class x) 
          do
          (remove-direct-instance instance x)))
          

(defun remove-instances-matching-spec (class inst-var spec rel)
  (if (variable? inst-var)
      (loop for instance in (get-current-direct-instances class)
            do
            (unless (eq (match-spec-against-instance instance inst-var spec)
                        :fail)
              (remove-direct-instance instance (name instance) rel class))
            finally
            (loop for subclass in (current-direct-subclasses class)
                  do
                  (remove-instances-matching-spec subclass inst-var spec (name subclass))))
      (loop for instance in  (find-all-current-instances-of-class-structure-named-x  
                              class inst-var)
            do
            
        (unless  (eq (match-spec-against-instance-slots  instance  spec)
                     :fail)
          (remove-direct-instance instance inst-var)))))
          

;;;FIND-INSTANCE ---

;;;;(defun find-instance (Name &optional parent)
;;;;  (find-current-instance name parent))

;;;FIND-CURRENT-INSTANCE --- Finds an instance named <name>.  If <parent> is given, then teh function
;;;looks for a direct instance. Otherwise it returns one of (possibly many) instances. 
;;This function should be used with caution: if multiple instances of a class exist,
;;;but no direct instance, then it is not possible to predict its behaviour.
;;;Finds an instance named <name>.  If <class-name> is given, then
;;;the instance is searched among the instances of <class-name>.  Note that this function
;;;can retrieve an instance from a subclass of <class-name> - i.e. it searches for instances
;;;not for direct instances.
;;;Note also that it is possible for a class to have two instances with the same name.
;;;In this case the first one which is found will be retrieved will be retrieved
;;;If <class-name> is not given then the current model is searched until the first instance
;;;named <name> is found
(defun find-current-instance (name &optional class-name)
  (cond (class-name
         (let ((class (get-domain-class class-name)))
           (if class
             (or (find-current-direct-instance class name)
                 (car
                  (find-all-current-instances-of-class-structure-named-x class name)))
             (error "Cannot find instance ~s of class ~s..class ~s does not exist in the current ontology"
                    name class-name class-name))))
        (t
         (block found
             (maphash #'(lambda (key class &aux result)
                          (declare (ignore key))
                           (setf result 
                                 (find-current-direct-instance class name))
                           (when result
                              (return-from found result)))
                *domain-classes*)))))

;;;FIND-ALL-CURRENT-INSTANCES-NAMED-X - Returns all the instances currently visible
;;;with the same name
(defun find-all-current-instances-named-x (x &aux result)
  (maphash #'(lambda (key class &aux inst)
               (declare (ignore key))
               (setf inst 
                     (find-current-direct-instance class x))
               (when inst
                 (push inst result)))
           *domain-classes*)
  result)




;;;*********************************************
;;;SLOT ACCESS AND MODIFICATION
;;;The filler of a domain slot is a four element list with the following format
;;;(<all-values> <local-values> <flag> <values-inferred-by-renaming>).  
;;;The flag indicates whether default values
;;;are being used. If this is the case <local-values> must be nil.
;;;<values-inferred-by-renaming> are local values which have not
;;;been explicitly asserted but have been inferred by the renaming 
;;;mechanism.
;;;A domain slot can never be unbound.

;;;DOMAIN-SLOTS
(defmethod domain-slots ((instance basic-domain-class))
  (domain-slots (parent-class instance))) ;;;(get-domain-class (type-of instance))))


;;;GET-LOCAL-SLOT-VALUES ---
(defmethod get-local-slot-values ((instance basic-domain-class)slot)
  (second (slot-value instance slot)))

;;;GET-LOCAL-SLOT-VALUES-NO-RENAMING 
(defun get-local-slot-values-no-renaming (instance slot)
 (let ((slot-info (slot-value instance slot)))
   (filter (second slot-info)
          #'(lambda (x)
              (not (member x (fourth slot-info)))))))

;;;GET-SLOT-VALUES ---
;;;This is the function to call to get all current slot values
(defmethod get-slot-values ((instance basic-domain-class)slot)
  (first (slot-value instance slot)))

;;;LOCAL-SLOT-VALUE-ALREADY-EXISTS? 
(defun local-slot-value-already-exists? (instance slot value)
  (let ((slot-info (slot-value instance slot)))
    (values (member value (second slot-info) :test #'equal)
            (member value (fourth slot-info)))))

(defmethod GET-INSTANCE-SLOT-VALUES  ((instance basic-domain-class)slot)
  (first (slot-value instance slot)))

;;;
(defmethod THE-INSTANCE-SLOT-VALUE ((instance basic-domain-class)slot)
  (let ((values (get-slot-values instance slot)))
    (if values
      (car values)
      :nothing)))

;;;Nobody calls this - commented out - Enrico 17/2/99
;(defmethod REMOVE-INSTANCE-SLOT-VALUE ((instance basic-domain-class)slot value)
;  (destructuring-bind (all-values &optional local-values flag)
;                      (or (slot-value instance slot)
;                          '(nil))
;     flag ;;;ignore
;     (when (member value local-values :test #'equal)
;       (remove-slot-value instance slot value all-values local-values ;;;;flag 
;                          t))))

;;;GET-INHERITED-SLOT-VALUES
(defmethod get-inherited-slot-values  ((instance basic-domain-class)slot)
  (let ((slot-info (slot-value instance slot)))
    (set-difference (first slot-info)(second slot-info))))

;;;MAYBE-ADD-SLOT-VALUE-TO-INSTANCE
;;;This is called when a local slot value is added. It checks whether the value is really new.
;;;If it is, it calls add-slot-value-to-instance-int.
(defmethod maybe-add-slot-value-to-instance ((instance basic-domain-class)
                                             slot value 
                                             &optional inferred-by-renaming?
                                             (check-cardinality? t)
                                             (propagate-renaming-locally? t))
  (multiple-value-bind (flag derived-through-renaming?)
                       (local-slot-value-already-exists? instance slot value)
    (cond ((null flag)
           (add-slot-value-to-instance-int instance slot value 
                                           inferred-by-renaming?
                                           check-cardinality?
                                           propagate-renaming-locally?))
          ((and derived-through-renaming?
                (not inferred-by-renaming?))
           (destructuring-bind (all-values  local-values flag2 values-inferred-by-renaming)
                               (slot-value instance slot)
             flag2
             (setf (slot-value instance slot)
                   (list all-values
                         local-values
                         nil
                         (remove value values-inferred-by-renaming))))))))


;;;ADD-SLOT-VALUE-TO-INSTANCE-INT
;;;Adds a new local value to a slot.  The function assumes that the value does
;;;not  exist already.
(defmethod add-slot-value-to-instance-int ((instance basic-domain-class)
                                           slot value &optional
                                           inferred-by-renaming?
                                           (check-cardinality? t)
                                           (propagate-renaming-locally? t)
                                           &aux new-values add-new-value?
                                           discarded-values)
  
  (destructuring-bind (&optional all-values local-values flag
                                 values-inferred-by-renaming)
                      (slot-value instance slot)
    (setf add-new-value? (not (member value all-values :test #'equal)))
    (setf new-values (List value))
    (cond (flag                               ;;are we using default values?
           (setf (slot-value instance slot)   ;;Yes, therefore we override them with the
                 (list new-values             ;;new local value
                       new-values
                       nil                     ;;flag set to nil signifies  that we are
                                               ;;not using default values
                       (when inferred-by-renaming?
                         new-values))
                 ;;;;;;; change? t
                 discarded-values (set-difference all-values new-values)))
          (add-new-value?
           ;;value is completely new
           (setf (slot-value instance slot)
                 (list (cons value all-values)
                       (cons value local-values)
                       nil
                       (if inferred-by-renaming?
                         (cons value values-inferred-by-renaming)
                         values-inferred-by-renaming))))
          (t
           ;;<value> was previously inherited. We update only the
           'local-values' field
           (setf (slot-value instance slot)
                 (list all-values
                       (cons value local-values)
                       nil
                       (if inferred-by-renaming?
                         (cons value values-inferred-by-renaming)
                         values-inferred-by-renaming)))))
    (when add-new-value?
      (tell-slot-values-to-fc-rules slot (name instance) (list value)
                                    (home-ontology instance)))
    ;;;;(tell-fc-rules slot (List (name instance) value))
    (when discarded-values
      (remove-slot-values-from-fc-rules slot (name instance)
                                        discarded-values
                                        (home-ontology instance)))
    (when (and check-cardinality?
               *check-cardinality*
               (or add-new-value? discarded-values))
      (check-cardinality instance slot (get-slot-values instance slot)))
    
    (when *check-constraints*
      (if discarded-values 
        (check-slot-assertion-constraints instance slot (get-slot-values instance slot))
        (if add-new-value?
          (check-slot-assertion-constraints instance slot (list value)))))

    (when (and propagate-renaming-locally?
               (not inferred-by-renaming?))
      (propagate-renaming-from-instance-slot instance slot value
                                             (renaming-chains
                                              (parent-class instance))))))
        
   

;;;MAYBE-REMOVE-SLOT-VALUE ---
;(defmethod maybe-remove-slot-value ((instance basic-domain-class)slot value &optional no-checks)
;   (destructuring-bind (all-values &optional local-values flag)
;       (or (slot-value instance slot)
;           '(nil))
;     flag ;;;ignore
;     (when (member value local-values :test #'equal)
;      (remove-slot-value instance slot value all-values local-values ;;;;flag 
;                          no-checks))))

;;;Changed to cope with renamed slots - Enrico 25/2/99
(defmethod maybe-remove-slot-value ((instance basic-domain-class) slot value)
  (when (local-slot-value-already-exists? instance slot value)
    (loop for each-slot in (collect-all-associated-slots-in-renaming-chains 
                            slot (renaming-chains (parent-class instance)))
          do
          (remove-slot-value-int instance each-slot value))
    (remove-slot-value-int instance slot value)))



;;;REMOVE-SLOT-VALUE-INT ---
;;;Removes a value from the list of local slot values.  The function assumes that
;;;the value is there
(defmethod remove-slot-value-int ((Instance basic-domain-class) slot value
                                  &aux change)
  (destructuring-bind (all-values local-values flag values-inferred-by-renaming)
                      (slot-value instance slot)
    flag ;;ignore
    (setf all-values (remove value all-values :test #'equal)
          local-values (remove value local-values :test #'equal)
          values-inferred-by-renaming (remove value  values-inferred-by-renaming :test #'equal))
    (if all-values
      (setf  (slot-value instance slot)
             (list all-values local-values nil values-inferred-by-renaming))
      (Let ((default-values (get-default-slot-values-from-class
                             (parent-class instance)
                             slot)))
	(setf (slot-value instance slot)
	      (list default-values nil (and default-values t) nil)
              change (set-difference  default-values (list value) :test #'equal))))
    (remove-slot-values-from-fc-rules slot (name instance) (list value)
                                      (home-ontology instance))
    (when change
      (tell-slot-values-to-fc-rules slot (name instance) change (home-ontology instance))
      (when *check-constraints*
           (check-slot-assertion-constraints instance slot (get-slot-values instance slot))
             ))
    (when *check-cardinality* ;;;)
      (check-cardinality instance slot (get-slot-values instance slot)))))

;;;REMOVE-LOCAL-SLOT-VALUES ---
(defmethod remove-local-slot-values ((instance basic-domain-class)slot)
  
  (loop for each-slot in (collect-all-associated-slots-in-renaming-chains 
                          slot (renaming-chains (parent-class instance)))
        do
        (remove-local-slot-values-int instance each-slot))
  (remove-local-slot-values-int instance slot))



(defmethod remove-local-slot-values-int ((instance basic-domain-class)slot
                                         ;;;;;&optional no-checks
                                         &aux change)

  (destructuring-bind (&optional  all-values local-values flag values-inferred-by-renaming)
                      (slot-value instance slot)
    (declare (ignore flag values-inferred-by-renaming))
    (when local-values 
      (if (set-equal all-values local-values :test #'equal)
          ;;No more 'definitional' values left.  Need to compute the default values
	  (Let ((default-values (get-default-slot-values-from-class                  
                                 (parent-class instance)
                                 slot
                                 )))
	    (setf (slot-value instance slot)       ;;Update the slot info
		  (list default-values 
                        nil
                        (and default-values t)
                        nil)
                  change                           ;;record eventual change for rule system
                  (set-difference  default-values local-values :test #'equal)))
          ;;There are definitional values left.  We only need to remove the local ones
	  (setf (slot-value instance slot)
		(list (set-difference all-values local-values :test #'equal)
                      nil
                      nil
                      nil)))
      (remove-slot-values-from-fc-rules slot (name instance)local-values
                                        (home-ontology instance))
      (when change
        (tell-slot-values-to-fc-rules slot (name instance) change (home-ontology instance))
         (when *check-constraints*
           (check-slot-assertion-constraints instance slot (get-slot-values instance slot))
             ))
      (when *check-cardinality* ;;;;;(and (not no-checks) 
        (check-cardinality instance slot (get-slot-values instance slot))))))


;;;CACHE-INHERITED-SLOTS ---Called after a new domain instance has been created.  It changes
;;;the slot value of an instance to a three element list (<all-values> <local-values> <flag>)
;;;where <all-values> is a list comprising all 'valid' values (local and inherited)
;;;<local-values> is a list comprising all locally defined values for the given slot.
;;;and <flag> is a boolean indicating whether <all-values> is made of inherited default values
(defmethod cache-inherited-slots ((instance basic-domain-class)(parent ocml-metaclass))
  (loop for slot in (domain-slots parent)
        for local-values = (when (slot-boundp instance slot)
                             (slot-value instance slot))
        for inherited-values = (get-slot-values-from-class-structure parent slot t )
        do
        (infer-slot-value instance parent slot local-values  inherited-values)
        finally
        (check-renaming-in-instance-slot-values instance parent)
        (when *check-cardinality*
          (loop for slot in (domain-slots parent)
                do
                (check-cardinality instance slot (get-slot-values instance slot))))))
        

      
;;;UPDATE-INSTANCE-AFTER-CLASS-REDEFINITION ---This method is called to recompute
;;;all the slot fillers of an instance after its parent class has been redefined
(defmethod update-instance-after-class-redefinition ((instance basic-domain-class)
                                                     (parent ocml-metaclass)
                                                     slots-to-recompute)
  (if (eq *current-ontology* (home-ontology instance))
    (update-instance-after-class-redefinition-internal instance parent slots-to-recompute)
    (let ((current *current-ontology*))
      (unwind-protect 
        (progn
          (switch-to-ontology (home-ontology instance))
          (update-instance-after-class-redefinition-internal instance parent slots-to-recompute))
        (unless (eq *current-ontology* current)
          (switch-to-ontology current))))))

(defun update-instance-after-class-redefinition-internal (instance parent slots-to-recompute)
  (loop for slot in slots-to-recompute
        for local-values = (when (slot-boundp   ;;The slot might be new and unbound!!
                                  instance slot)
                             (get-local-slot-values-no-renaming instance slot))
        for inherited-values = (get-slot-values-from-class-structure parent slot t )
        do
        (infer-slot-value instance parent slot local-values inherited-values)
        finally
        (check-renaming-in-instance-slot-values instance parent)
        (when *check-constraints*
          (check-instance-constraints instance parent)
          (check-instance-slot-constraints instance))
        (when *check-cardinality*
          (loop for slot in (domain-slots parent)
                do
                (check-cardinality instance slot (get-slot-values instance slot))))))




;;;INFER-SLOT-VALUE ---This method recomputes the slot filler of an instance.
(defmethod infer-slot-value ((instance basic-domain-class)(parent-class ocml-metaclass)
                             slot local-values inherited-values
			     &aux  new-values)
  (cond ((or inherited-values local-values) 
	 (setf (slot-value instance slot)   ;;;no need for default values
	       (list
                (setf new-values
                      (remove-duplicates
		       (append local-values inherited-values)
		       :test #'equal))
	        local-values
	        nil
                nil)))
        (t
         ;;We need to get the default values
         (setf new-values (get-default-slot-values-from-class
                           parent-class slot))
	 (setf (slot-value instance slot)
	       (when new-values
		 (list new-values
		       nil
		       t
                       nil))))))


;;;MATCH-SPEC-AGAINST-INSTANCE ---Matches an instance against a goal such as
;;;(<class> <inst-var> <slot1> <filler1> .......<slotn> <fillern>).  It returns
;;;multiple envs, or :fail.
(defun match-spec-against-instance (instance ;;;;;;(instance basic-domain-class)
					inst-var
					spec     ;;<spec> = (slot1 filler1 slot2.....)
					&optional env ;;;;;all?
					&aux result)
  (with-slots (name) instance
    (setf result (match name inst-var env))
    (cond ((eq result :fail)
           result)
          (spec
	   (match-spec-against-instance-slots instance spec result))
          (t
           (list result)))))


;;;MATCH-SPEC-AGAINST-INSTANCE-SLOTS ---Matches a spec such as (slot1 filler1 slot2.....)
;;;against a domain instance.  The function assumes that <spec> is not NIL.
;;;Returns a list of successful envs, or :fail
(defmethod match-spec-against-instance-slots ((instance basic-domain-class)
				              spec  ;;;<spec> = (slot1 filler1 slot2.....)
				              &optional env 
				             ;;;;;; (returned-envs (List env))
                                              )
  (Let ((new-envs (match-instance-slot instance (car spec) (second spec) env)))
    (if (eq new-envs :fail)
	new-envs
	(if (cddr spec)
	    (or 
	     (mapcan #'(lambda (x)
			 (let ((result (match-spec-against-instance-slots
					instance (cddr spec)x)))
			   (unless (eq result :fail)
			     result)))
		     new-envs)
	     :fail)
	    new-envs))))

;;;MATCH-INSTANCE-SLOT ---Returns multiple matches or :fail
(defmethod match-instance-slot ((instance basic-domain-class) slot arg &optional env)
  (loop with values = (get-slot-values instance slot)
          with result 
	   with match
	   for value in values
	   do
	   (setf match (match value arg env))
	   (unless (eq match :fail)
	     (push match result))
	   finally
	   (return (or result :fail))))
  
;;;CHECK-CARDINALITY
(defmethod check-cardinality ((instance  basic-domain-class)  slot values)
  (let* ((class (parent-class instance))
         (min-cardinality (get-min-cardinality class slot))
         (max-cardinality (get-max-cardinality class slot))
         (l (length values)))
    (when (or min-cardinality max-cardinality)
      (setf min-cardinality (or  min-cardinality 0)
            max-cardinality (or max-cardinality most-positive-fixnum))
      (when (or (< l min-cardinality)
                (> l max-cardinality))
        (if (= l 0)
          (ocml-warn "Slot ~s of ~S has no fillers and therefore violates minimal cardinality ~s"
                slot (name instance) min-cardinality)
          (ocml-warn "Slot ~s of ~S has ~s values~{, ~S~}, and therefore violates cardinality constraint <~S, ~S>"
                 slot (name instance) l values (or min-cardinality 0)
                 (or max-cardinality :any)))))))


(defmethod check-slot-type ((instance  basic-domain-class)  slot values
                            &optional (force-checking? nil))
  (let* ((*ignore-undefined-relations*  (if (and (boundp '*pending-constraints*)
                                                 (not force-checking?))
                                          t
                                          *ignore-undefined-relations*  ))
         (name (name instance))
         (class (parent-class instance))
         (types (get-slot-type class slot)))
    (loop with violations = nil
          for value in values 
          do
          (loop for type in types 
                for flag = (check-slot-type-internal type value) ;;;;;(holds?  type value)
                do
                (unless flag
                  (push (List value slot type) violations)))
          finally
          (when violations
            (if (and (boundp '*pending-constraints*)
                     (not force-checking?))
              (pushnew (list :instance-slot-type instance slot) *pending-constraints* :test #'equal)
              (loop for violation in violations
                    do
                    (ocml-warn "The value ~s of slot ~s of ~S violates type constraint ~s"
                               (car violation) (second violation) name (third violation))))))))

(defun check-slot-type-internal (type value)
  (let* ((rel  (get-relation type)))
    (when rel
      (Let ((suff-cond (sufficient-for-type-checking rel)))
        (cond (suff-cond
               (setf suff-cond `(kappa ,(schema rel) , suff-cond))
               (holds? suff-cond value))
              (t
               (holds? type value)))))))
      


    
    
  





