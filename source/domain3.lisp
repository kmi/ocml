;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


;;;*************************************************************
;;;;;;;;;;;;;;;;;;DOMAIN INSTANCES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun domain-instance? (thing)
  (typep thing 'basic-domain-class))


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
    (let ((instance (find-instance name))
	  (real-parent (internal-name class))) 
      (when instance
        (when (tracing-this-assertion? (List parent name))
              (print-with-spaces (1+ *task-level*)
                                 "Redefining instance ~s of class ~s" 
                                 name parent))
        (let ((*traced-assertions* nil))
        (remove-direct-instance instance name)))
      (setf instance
            (apply #'make-instance
                   `(,real-parent :name ,name ,@slots :documentation ,documentation)))
      (record-source-file name 'ocml-class-instance)
      (cache-inherited-slots instance class)
      (tell-fc-new-instance instance parent)
      instance))

(defun change-instance-parent-class (instance-name old-parent-name new-parent-name)
  (let ((Instance (find-instance instance-name old-parent-name))
        (old-parent (get-ocml-class old-parent-name))
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
                                             (documentation instance)
                                             spec))))
          (t
           (unless instance
             (error "cannot find instance named ~s" instance-name))
           (unless old-parent 
             (error "cannot find class named ~s" old-parent))
           (unless new-parent 
             (error "cannot find class named ~s" new-parent))))))
           

(defun rename-instance (old-name new-name)
  (let ((instance (find-instance old-name)))
    (cond (instance 
           (remove-direct-instance instance old-name)
           (setf (name instance) new-name)
           (add-instance-to-class (parent-class instance) instance new-name)
           (tell-fc-new-instance instance (name (parent-class instance))))           
          (t
           (error "~s is not an instance" old-name)))))
  


;;;MAYBE-REMOVE-DOMAIN-INSTANCE -- Kills an instance named <name> if it is an instance
;;; of <parent>.
(defun maybe-remove-domain-instance (name class)
  (let ((instance (find-instance-of-class class  name)))
    (when instance
      (remove-direct-instance instance name ))))


;;;REMOVE-DOMAIN-INSTANCE-GEN ---Removes all instances of class, which match
;;;inst-term.
(defun remove-domain-instance-gen (inst-term parent class)
  (if (variable? inst-term)
      (remove-all-instances class parent)                  
      (maybe-remove-domain-instance inst-term class)))

(defun remove-instances-matching-spec (class inst-var spec rel)
  (if (variable? inst-var)
      (loop for instance in (get-direct-instances class)
            do
            (unless (eq (match-spec-against-instance instance inst-var spec)
                        :fail)
              (remove-direct-instance instance (name instance) rel class))
            finally
            (loop for subclass in (subclasses rel class)
                  do
                  (remove-instances-matching-spec subclass inst-var spec (name subclass))))
      (let ((instance (find-instance-of-class class  inst-var)))
        (when (and instance
                   (not (eq (match-spec-against-instance-slots  instance  spec)
                        :fail)))
          (remove-direct-instance instance inst-var))))) 
          
;;;FIND-INSTANCE ---Finds an instance named <name>.  If <parent> is given, then
;;;The instance is searched among its instances.
(defun find-instance (Name &optional parent)
  (if parent
      (find-instance-of-class (get-domain-class parent) name)
     (block found
             (maphash #'(lambda (key class &aux result)
                          (declare (ignore key))
                           (setf result 
                                 (find-direct-instance class name))
                           (when result
                              (return-from found result)))
                *domain-classes*))))
;;;       (loop for class being the hash-value in *domain-classes*
;;;             ;;;;;;for structure = (find-class class)
;;;             for result = (find-instance-in-class class name)
;;;             until result
;;;             finally
;;;             (return result))))

;;;*********************************************
;;;SLOT ACCESS AND MODIFICATION
;;;The filler of a domain slot is a three element list with the following format
;;;(<all-values> <local-values> <flag>).  The flag indicates whether default values
;;;are being used. If this is the case <local-values> must be nil.
;;;A domain slot can never be unbound.

;;;DOMAIN-SLOTS
(defmethod domain-slots ((instance basic-domain-class))
  (domain-slots (parent-class instance))) ;;;(get-domain-class (type-of instance))))


;;;GET-LOCAL-SLOT-VALUES ---
(defmethod get-local-slot-values ((instance basic-domain-class)slot)
  (second (slot-value instance slot)))

;;;GET-SLOT-VALUES ---
;;;This is the function to call to get all current slot values
(defmethod get-slot-values ((instance basic-domain-class)slot)
  (first (slot-value instance slot)))


(defmethod GET-INSTANCE-SLOT-VALUES  ((instance basic-domain-class)slot)
  (first (slot-value instance slot)))

;;;
(defmethod THE-INSTANCE-SLOT-VALUE ((instance basic-domain-class)slot)
  (let ((values (get-slot-values instance slot)))
    (if values
      (car values)
      :nothing)))

(defmethod REMOVE-INSTANCE-SLOT-VALUE ((instance basic-domain-class)slot value)
  (destructuring-bind (all-values &optional local-values flag)
                      (or (slot-value instance slot)
                          '(nil))
     flag ;;;ignore
     (when (member value local-values :test #'equal)
       (remove-slot-value instance slot value all-values local-values ;;;;flag 
                          t))))

;;;GET-INHERITED-SLOT-VALUES
(defmethod get-inherited-slot-values  ((instance basic-domain-class)slot)
  (let ((slot-info (slot-value instance slot)))
    (set-difference (first slot-info)(second slot-info))))

;;;ADD-SLOT-VALUE-TO-INSTANCE
;;;This is called when a local slot value is added
(defmethod add-slot-value-to-instance ((instance basic-domain-class)
                                       slot value &aux new-values change? discarded-values)
  (destructuring-bind (all-values  &optional local-values flag)
        (or (slot-value instance slot)
            '(nil))
      (setf new-values (List value))
    (cond (flag                               ;;are we using default values?           
	   (setf (slot-value instance slot)   ;;Yes, therefore we override them with the 
		 (list new-values             ;;new local value
		       new-values
		       nil)                   ;;flag set to nil signifies that we are
		 change? t                    ;;not using default values
                 discarded-values (set-difference all-values new-values)))
	  ;;;No default values.  We first check whether <value> is not already there
          ((not (member value local-values
                        :test #'equal))
	   (if (member value all-values)   
	       (setf (slot-value instance   ;;<value> was previously inherited.  This means
                                 slot)	    ;;we update the 'local-values' field
                     (list all-values
                           (cons value local-values) nil))
               (setf (slot-value instance slot)
                     (list (cons value all-values)
                           (cons value local-values) nil)
                     change? t))))
    (when change?
      (tell-slot-values-to-fc-rules slot (name instance) (list value))
      ;;;;(tell-fc-rules slot (List (name instance) value))
      (when discarded-values
        (remove-slot-values-from-fc-rules slot (name instance) discarded-values))
      (when *check-cardinality*
        (check-cardinality instance slot (get-slot-values instance slot))))))



;;;MAYBE-REMOVE-SLOT-VALUE ---
(defmethod maybe-remove-slot-value ((instance basic-domain-class)slot value &optional no-checks)
   (destructuring-bind (all-values &optional local-values flag)
       (or (slot-value instance slot)
           '(nil))
     flag ;;;ignore
     (when (member value local-values :test #'equal)
       (remove-slot-value instance slot value all-values local-values ;;;;flag 
                          no-checks))))


;;;REMOVE-SLOT-VALUE ---
;;;Removes a value from the list of local slot values.  The function assumes that
;;;the value is there
(defmethod remove-slot-value ((Instance basic-domain-class)slot value
                              all-values local-values ;;;flag
                              &optional no-checks
                              &aux change)
  (setf all-values (remove value all-values :test #'equal)
        local-values (remove value local-values :test #'equal))
  (if all-values
      (setf  (slot-value instance slot)
             (list all-values local-values nil))
      (Let ((default-values (get-default-slot-values-from-class
                             (parent-class instance)
                             slot
                             )))
	(setf (slot-value instance slot)
	      (list default-values nil (and default-values t))
              change (set-difference  default-values (list value) :test #'equal))))
  (remove-slot-values-from-fc-rules slot (name instance) (list value))
  ;;;; (unassert-from-fc-rules slot (list (name instance)value))
   (when change
        (tell-slot-values-to-fc-rules slot (name instance) change))  
  (when (and (not no-checks)*check-cardinality*)
    (check-cardinality instance slot (get-slot-values instance slot))))

;;;REMOVE-LOCAL-SLOT-VALUES ---
(defmethod remove-local-slot-values ((instance basic-domain-class)slot
                                     &optional no-checks
                                     &aux change)
  (destructuring-bind (all-values &optional local-values flag)
      (or (slot-value instance slot)
          '(nil))
    (declare (ignore flag))
    (when local-values 
      (if (set-equal all-values local-values :test #'equal)
          ;;No more 'definitional' values left.  Need to compute the default values
	  (Let ((default-values (get-default-slot-values-from-class                  
                                 (parent-class instance)
                                 slot
                                 )))
	    (setf (slot-value instance slot)       ;;Update the slot info
		  (list default-values nil
                        (and default-values t))
                  change                           ;;record eventual change for rule system
                  (set-difference  default-values local-values :test #'equal)))
          ;;There are definitional values left.  We only need to remove the local ones
	  (setf (slot-value instance slot)
		(list (set-difference all-values local-values :test #'equal)
                      nil
                      nil)))
      (remove-slot-values-from-fc-rules slot (name instance)local-values)
      (when change
        (tell-slot-values-to-fc-rules slot (name instance) change))
      (when (and (not no-checks) *check-cardinality*)
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
        (infer-slot-value instance parent slot local-values  inherited-values)))

;;;INFER-SLOT-VALUE ---This method recomputes the slot filler of an instance.
(defmethod infer-slot-value ((instance basic-domain-class)(parent-class ocml-metaclass)
                             slot local-values inherited-values
			     &aux  new-values)
  (cond((or inherited-values local-values) 
	(setf (slot-value instance slot)   ;;;no need for default values
	      (list
               (setf new-values
                     (remove-duplicates
		      (append local-values inherited-values)
		      :test #'equal))
	       local-values
	       nil)))
       (t
        ;;We need to get the default values
        (setf new-values (get-default-slot-values-from-class
                                parent-class slot))
	(setf (slot-value instance slot)
	      (when new-values
		(list new-values
		      nil
		      t)))))
      
  ;(when new-values               ;;;This is now done by tell-fc-instance-of-links
;    (tell-slot-values-to-fc-rules slot (name instance) new-values))
  (when *check-cardinality*
    (check-cardinality instance slot new-values)))

;;;UPDATE-INSTANCE-AFTER-CLASS-REDEFINITION ---This method is called to recompute
;;;all the slot fillers of an instance after its parent class has been redefined
(defmethod update-instance-after-class-redefinition ((instance basic-domain-class)
                                                     (parent ocml-metaclass)
                                                     slots-to-recompute)
  (loop for slot in slots-to-recompute
        for local-values = (when (slot-boundp   ;;The slot might be new and unbound!!
                                  instance slot)
                             (get-local-slot-values instance slot))
        for inherited-values = (get-slot-values-from-class-structure parent slot t )
        do
        (infer-slot-value instance parent slot local-values inherited-values)))


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
(defmethod check-cardinality ((instance  basic-domain-class)slot values)
  (let* ((class (parent-class instance))
         (min-cardinality (get-min-cardinality class slot))
         (max-cardinality (get-max-cardinality class slot))
         (l (length values)))
    (when (or min-cardinality max-cardinality)
      (setf min-cardinality (or  min-cardinality 1)
            max-cardinality (or max-cardinality most-positive-fixnum))
      (if (or (< min-cardinality l)
              (> l max-cardinality))
          (warn "The value ~s of slot ~s of ~S violates cardinality constraint <~S, ~S>"
                values slot instance (or min-cardinality 0)
                (or max-cardinality :any))))))



