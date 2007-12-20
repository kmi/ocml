(in-package :ocml)

;;;INSTANCES-META
(defclass instances-meta (standard-class)
  ((all-instances-table :initform (make-hash-table :test #'equal) 
                        :accessor class-all-instances-table
                        :documentation 
                        "A hash table storing all instances of the class, whether direct or inherited")
   (direct-instances-list :initform nil 
                          :accessor class-direct-instances-list)))

(defun initialize-instances (class)
  (when (ocml-class? class)
    (with-slots (all-instances-table direct-instances-list) class
      (setf all-instances-table (make-hash-table)
           direct-instances-list nil))))
          

;;;GET-CLASS-INSTANCES-NAMED-X
(defun get-class-instances-named-x (class name)
  (with-slots (all-instances-table) class
    (gethash name all-instances-table)))


;;;FIND-CURRENT-DIRECT-INSTANCE - returns the current direct instance named <name> in 
;;;class <class>.  Only one current direct instance can exist with a given name 
(defun find-current-direct-instance (class name)
  (with-slots (all-instances-table) class
    (find class (filter-current-instances 
                 (gethash name all-instances-table))
          :test #'(lambda (x y)
                    x
                    (eq (parent-class y) class)))))



;;;FIND-ALL-CURRENT-INSTANCES-OF-CLASS-STRUCTURE-NAMED-X ---Returns an instance (not necessarily a direct instance!!!) of a class.
(defun find-all-current-instances-of-class-structure-named-x (class name)
  (with-slots (all-instances-table) class
    (filter-current-instances 
     (gethash name all-instances-table))))


;;;FIND-ALL-CURRENT-INSTANCES-OF-CLASS-NAMED-X --Finds all teh current instances of a class
;;;with a given name.  Note that while there can only be one direct instance of a class with
;;;a specific name, it is possible to find more than one instance with the same name
;;;New definition: it does not need to search the sublcass tree anymore, 
;;;it simply access the all-instances-table - EM, 20/9/02

(defun find-all-current-instances-of-class-named-x (x class-name)
  (let ((class (get-domain-class class-name)))
    (if class
      (with-slots (all-instances-table) class
        (filter-current-instances (gethash x all-instances-table)))
      (error "Class ~s does not exist in the current ontology"
             class-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;GET-DIRECT-INSTANCES
(defun get-direct-instances (class) 
   (class-direct-instances-list class))

;;;GET-CURRENT-DIRECT-INSTANCES 
(defun get-current-direct-instances (class)
 (filter-current-instances (get-direct-instances class)))
  

;;;GET-ALL-INSTANCES
;;(defun get-all-instances  (class);;;;;((class instances-meta))
;;  (remove-duplicates (get-all-instances2 class)))

;;(defun get-all-instances2 (class)
;;  (append (get-direct-instances class)
;;          (mapcan #'get-all-instances2 (subclasses class))))

(defun get-all-instances  (class &aux result)
  (with-slots (all-instances-table) class
     (maphash #'(lambda (key values)
                  key
                  (setf result (append values result)))
              all-instances-table)
     result))

;;;GET-CURRENT-INSTANCES                                                                
(defun get-current-instances (class)
  (filter-current-instances (get-all-instances class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;GET-ALL-INSTANCES-IN-CLASSES
(defun get-all-instances-in-classes (classes)
  (remove-duplicates (mapcan #'get-all-instances classes)))

;;;FIND-ALL-CURRENT-INSTANCES-IN-CLASSES
(defun find-all-current-instances-in-classes (classes)
  (remove-duplicates (mapcan #'(lambda (class)(get-current-instances class))classes)))

  
;;;GET-ALL-INSTANCES-NAMED-X-IN-CLASSES
(defun get-all-instances-named-x-in-classes (name classes)
  (remove-duplicates (mapcan #'(lambda (class)
                                 (get-class-instances-named-x class name))
                             classes)))


;;;FIND-ALL-CURRENT-INSTANCES-NAMED-X-OF-THESE-CLASSES
(defun find-all-current-instances-named-x-of-these-classes (x classes)
  (loop with result 
        for class-name in classes
        for instances = (find-all-current-instances-of-class-named-x x class-name)
        do
        (setf result (append result instances))
        finally 
        (return (remove-duplicates result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;REMOVE-ALL-CURRENT-INSTANCES
(defun remove-all-current-instances (class class-name)
  (loop for instance in (get-current-direct-instances class)
	do
	(remove-direct-instance instance (name instance) class-name class)
	finally
	(loop for subclass in (current-subclasses class)
              do
              (loop for s-instance in (get-current-direct-instances subclass)
                  do
                  (remove-direct-instance s-instance (name s-instance) (name subclass) subclass)))))



;;;ADD-INSTANCE-TO-CLASS
;;;Adds an instance to its class instances.  If it is a new one,
;;;the rule system is informed.
;;;Now you can have multiple instances of a class with the same name!
;;;but these have to be in different ontologies - Enrico 23/6/99

;;(defmethod add-instance-to-class ((class instances-meta)instance name)
;;  (with-slots (instances  instances-list) class
;;    (push instance  instances-list)
;;    (push instance (gethash name instances))))

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

   

;;;PARENT-CLASS
(defun parent-class (instance)
  (find-class (type-of instance)))


;;;REMOVE-DIRECT-INSTANCE --
(defun remove-direct-instance (instance name &optional class-name class)
  "Physically removes an instance from its parent class.  CLASS-NAME
is the external, rather than internal, name of the class."
  (setf class (or class
                  (parent-class instance))
        class-name (or class-name
                       (name class)))
  (remove-direct-instance-internal name instance class))


;;;REMOVE-DIRECT-INSTANCE-INTERNAL
(defun remove-direct-instance-internal (name instance class)
  (with-slots (all-instances-table direct-instances-list) class 
    (Setf direct-instances-list (remove instance  direct-instances-list))
    (setf (gethash name all-instances-table)
          (remove instance (gethash name all-instances-table))))
  (loop for super in (domain-superclasses class)
          do
          (remove-instance-from-superclass-hash-table instance name super))
  (remove-all-instance-info-from-fc-rules instance (name class)))


(defun remove-instance-from-superclass-hash-table (instance name class)
  (with-slots (all-instances-table ) class
    (setf (gethash name all-instances-table)
          (remove instance (gethash name all-instances-table)))))


;;;FILTER-ACTIVE-CLASSES
(defun filter-active-classes (classes)
  "filters out those classes which are not active in the current ontology - i.e. they have been superseded
   by definitions with the same name"
  (filter classes
          #'(lambda (class)
              (eq class (get-domain-class (name class))))))


;;;FILTER-CURRENT-INSTANCES - filter the instances in a list which are 
;;;in one of teh current ontologies and are not overridden
(defun filter-current-instances (instances)
  (filter  instances
           #'(lambda (inst)
               (and (member  (home-ontology inst)
                             *current-ontologies*)
                    (or (null (instance-overridden-by inst))
                        (not (some  #'(lambda (inst2)
                                        (and (member inst2 instances)
                                             (member  (home-ontology inst2)
                                                      *current-ontologies*)))
                                    (instance-overridden-by inst))))))))
  


(defun remove-all-slot-values-from-all-instances-of-these-classes (classes slot)
  (loop for instance in (get-all-instances-in-classes classes )
        do
        (remove-local-slot-values instance slot)))

(defun remove-slot-value-from-all-instances-of-these-classes (classes slot value)
  (loop for instance in (get-all-instances-in-classes classes )
        do
        (maybe-remove-slot-value instance slot value)))

(defun remove-all-slot-values-from-all-instances-named-x-of-these-classes (name classes slot)
  (loop for instance in (get-all-instances-named-x-in-classes name classes )
        do
        (remove-local-slot-values instance slot)))

(defun remove-slot-value-from-all-instances-named-x-of-these-classes (name classes slot value)
  (loop for instance in (get-all-instances-named-x-in-classes name classes )
        do
        (maybe-remove-slot-value instance slot value)))

;; This is a boring thing you have to do to ensure your metaclass
;; definition is accepted.
(defmethod
    #+:allegro mop:validate-superclass
    #+:lispworks clos::validate-superclass
    #+:sbcl sb-mop:validate-superclass
    ((class instances-meta) (superclass standard-class))
    (declare (ignore class superclass))
    t)

;;;OCML-METACLASS 
(defclass almost-ocml-metaclass (instances-meta basic-ocml-object)
  ((instance-var :initarg :instance-var)
   (ocml-name  :accessor name)
   ;;;;;(methods :initform nil :accessor methods)
   (domain-slots :accessor domain-slots)            ;;;all domain slots (local + inherited)
   (local-slots :accessor local-slots :initform nil);;;all local slots
   (own-slots :initarg :own-slots :accessor own-slots  :initform nil)
   (slots-as-relations :initform nil :accessor slots-as-relations)
   (virtual-slots :initform nil            ;;;these are binary relations whose first 
                  :accessor virtual-slots) ;;;argument has <this class> as type
                  
   (ocml-options :accessor ocml-options)
   (ocml-options-no-renaming 
    :accessor ocml-options-no-renaming
    :documentation "The OCML options as specified at source, with no inferred renaming")
   (slot-info-alist :initform nil :accessor slot-info-alist)
   (renaming-chains           ;;;These are the renaming chains applicable to this class
    :accessor renaming-chains) ;;;including those inherited from superclasses
   (effective-renaming-chains   ;;;These are the effective renaming chains - i.e. those to be used for 
                                ;;;inferencing
    :accessor effective-renaming-chains)
   (renaming-pairs            
    :accessor renaming-pairs  ;;;These are the locally defined renaming pairs for a 
    :initform nil             ;;;class, which make sense - i.e., they have been checked
    )))

(defclass ocml-metaclass (almost-ocml-metaclass)
  ())

(defmethod internal-name ((class ocml-metaclass))
  (class-name class))

;;; {{{ Subsumption
(defmethod direct-domain-superclasses ((class ocml-metaclass))
  (remove (find-class 'basic-domain-class)(direct-superclasses nil class)))

(defmethod domain-superclasses ((class ocml-metaclass))
  (remove class
          (filter (class-precedence-list class)
                  #'domain-class?)))

(defun current-direct-subclasses (class)
   (filter (direct-subclasses  class)
           #'(lambda (sub)
               (member  (home-ontology sub)
                          *current-ontologies*))))

(defun current-subclasses (class)
   ;;returns all subclasses of class which 
  ;;are in a currently selected ontology
   (filter  (subclasses  class)
           #'(lambda (sub)
               (member  (home-ontology sub)
                          *current-ontologies*))))

(defun superclass-of* (class classes)
  (let ((subs (subclasses class)))
    (when subs
      (some  #'(lambda (class2)
                 (member class2 subs))
             classes))))

(defun subclass-of* (class classes)
  (let ((supers (domain-superclasses class)))
    (when supers
      (some  #'(lambda (class2)
                 (member class2 supers))
             classes))))

(defun remove-subsumed-classes (classes)
  "Return a list of CLASSES, without those which have a superclass in
CLASSES."
  (remove-if* #'subclass-of* classes))

(defun remove-subsuming-classes (classes)
  "Return a copy of CLASSES without those which are superclasses of
others."
  (remove-if* #'superclass-of* classes))

(defun remove-if* (test list)
  "Return a copy of LIST without elements satisfying (test el LIST)."
  (loop for el in list
     when (not (funcall test el list))
     collect el))

(defun common-ancestor (objects top-class)
  (let ((result (first (if (= (length objects) 1)
			   objects
			   (apply #'intersection*
				  (mapcar #'instance-superclasses objects))))))
    (unless (eq result top-class)
      result)))

(defun instance-superclasses (instance)
  (ocml-eval-gen `(setofall ?x (instance-of ,instance ?x))))
;;; }}}

(defmethod inherited-slots ((class ocml-metaclass))
  (with-slots (domain-slots local-slots)
      class
  (set-difference domain-slots local-slots)))


(defun ocml-class? (class)
  (typep class 'ocml-metaclass))

(defun set-own-slots (class new-own-slots )
  (with-slots (ocml-name own-slots) class
    (unassert-own-slots ocml-name class own-slots)
    (Loop for slot-spec in new-own-slots
          do
          (destructuring-bind (slot &rest values) slot-spec
            (Let ((*asserting-own-slots* t))
              (loop for value in values
                    do
                    (tell1 `(,slot ,ocml-name ,value)))
              (add-own-slot-of slot class))))
    (setf own-slots new-own-slots)))

(defun unassert-own-slots (name class own-slots)
  (Loop for slot-spec in own-slots
          do
          (destructuring-bind (slot &rest values) slot-spec
            (loop for value in values
                    do
                    (unassert1 `(,slot ,name ,value)))
            (remove-own-slot-of slot class))))

;;;SET-METACLASS-INFORMATION
(defmethod set-metaclass-information ((class ocml-metaclass)
                                      domain-slots1        ;;;all domain slots (Local + inherited)
                                      ocml-slots-info      ;;;all ocml slot info (local + inherited)
                                      local-slots1 
                                      ocml-options1  
                                      &optional 
                                      own-slots
                                      name 
                                      instance-var1)
  (with-slots (ocml-name domain-slots local-slots instance-var ;;;;onto-spec
                               ocml-options slot-info-alist)
              class
    (setf
     domain-slots domain-slots1 
     slot-info-alist ocml-slots-info
     local-slots local-slots1
     ocml-options ocml-options1)
    (when name
      (setf  
       ocml-name name 
       instance-var instance-var1))
    (when own-slots
      (set-own-slots class own-slots))))

;;;CLASS-INHERITED-SLOTS ---Returns all the slots inherited from a list of (super-)classes
(defun class-inherited-slots (superclasses)
  (remove-duplicates 
   (loop for class in superclasses
	 appending (domain-slots class))))


;;;FIND-OPTION-VALUE ---
(defmethod find-option-value ((class ocml-metaclass)slot option)
  (with-slots (slot-info-alist) class
    (let ((structure (right-value slot slot-info-alist)))
      (when structure
        (get-slot-info-from-structure option structure)))))

;;;(defmethod find-option-value ((class (eql (find-class 'basic-domain-class))) slot option)
;;;  slot option
;;;  nil)

         
(defun find-ocml-options-value (class slot option-to-find)
  (let ((options (right-value slot
                              (ocml-options class))))
    (mapcan #'(lambda (option)
                (when (eq (car option) option-to-find)
                  (list (second option))))
            options)))

;;;ADD-SLOT-OF-LINKS
(defmethod add-slot-of-links ((class ocml-metaclass))
  (with-slots (domain-slots slots-as-relations  local-slots)class
    (setf slots-as-relations nil)
    (loop for slot in domain-slots
          for relation = (find-or-create-relation slot 2)
 	  do
          (add-slot-of-entry  relation class)
          (push relation slots-as-relations)
          (when (member slot local-slots)
            (add-local-slot-of-entry relation class)))))



;;;ENSURE-CLASS-IS-RELATION ---This function is called to ensure that when a class
;;;is created, the associated unary relation is created as well, if it does not exist
;;;already.  redefined 23-6-99 (enrico)



;;redefined 3-12-99 - enrico
;(defun ensure-class-is-relation (name instance-var relation-spec &aux rel-instance)
;  (setf rel-instance (get-relation name))
 ; (cond ((not rel-instance)
 ;        (make-ocml-relation-internal name (append 
 ;                                           (list :schema (list instance-var)
;                                                    :defined-from-class? t)
;                                            relation-spec)))
;        ((or relation-spec
;             (defined-from-class? rel-instance))
;         ;;we redefine the relation
;         (make-ocml-relation-internal name (append 
 ;                                             (list :schema (list instance-var)
 ;                                                   :defined-from-class? t)
;                                              relation-spec)
;                                      nil nil (defined-by-rule rel-instance)))
;        (t
;         ;;we only need to clear the relation-instances
;         (ocml-warn "Relation ~s being redefined as a class...deleting existing relation instances"
;                    name)
;         (setf (relation-instances rel-instance) nil))))

 ;;redefined 4-6-04 - enrico        
(defun ensure-class-is-relation (name instance-var relation-spec &aux rel-instance)
  (setf rel-instance (get-relation name))
  (cond ((not rel-instance)
         (make-ocml-relation-internal name (append
                                            (list :schema (list (or instance-var (gentemp "?VAR")))
                                                    :defined-from-class? t)
                                            relation-spec)))
        ((or relation-spec
             (defined-from-class? rel-instance))
         ;;we redefine the relation
         (make-ocml-relation-internal name (append
                                              (list :schema (list (or instance-var (gentemp "?VAR")))
                                                    :defined-from-class? t)
                                              relation-spec)
                                      nil nil (defined-by-rule rel-instance)))
        (t
         ;;we only need to clear the relation-instances
         (ocml-warn "Relation ~s being redefined as a class...deleting existing relation instances"
                    name)
         (setf (relation-instances rel-instance) nil))))

(defmethod get-min-cardinality ((class t) slot)
  (find-option-value (get-domain-class class) slot :min-cardinality))

(defmethod get-max-cardinality ((class t) slot)
  (find-option-value (get-domain-class class) slot :max-cardinality))

(defmethod get-min-cardinality ((class ocml-metaclass) slot)
  (find-option-value class slot :min-cardinality))

(defmethod get-max-cardinality ((class ocml-metaclass) slot)
  (find-option-value class slot :max-cardinality))

;;;GET-SLOT-TYPE ---Returns all the types applicable to <slot> of <class>
(defun get-slot-type (class slot)
  (find-option-value class slot :type))

;;;GET-SLOT-VALUES-FROM-CLASS
;(defun get-slot-values-from-class (name slot &optional no-defaults)
;  (let ((class (get-domain-class name)))
;    (if class
;        (get-slot-values-from-class-structure class slot no-defaults)
;	(error "~S is not a class" name))))


(defun all-class-slot-values (name slot)
  "Returns all slot values for a class slot.  If both defaults
   and definitional values are specified, only definitional
   ones are returned"
  (let ((class (get-domain-class name)))
    (if class
      (multiple-value-bind (values defaults)
                           (get-slot-values-from-class-structure class slot)
        (or values
            defaults))
      (error "~S is not a class" name))))

(defun all-class-slot-local-values (name slot)
  "Same as all-class-slot-values but returns a value if the slot is
local."
  (let ((class (get-domain-class name)))
    (if class
        (when (find slot (local-slots class))
          (multiple-value-bind (values defaults)
              (get-local-slot-values-from-class-structure 
               class slot)
            (or values
                defaults)))
      (error "~S is not a class" name))))




;;;GET-SLOT-VALUES-FROM-CLASS-STRUCTURE
;;;This function returns two values:
;;;- The inherited :Value fillers
;;;- The 'applicable' default values.  
;;;Applicability here is determined by the inheritance flag
;;;If no-defaults is T, the only :value options are returned
(defun get-slot-values-from-class-structure (class slot &optional no-defaults )
  (values
   (find-option-value class slot :value)
   (unless no-defaults
     (find-option-value  class slot :default-value))))

       
(defun get-local-slot-values-from-class-structure 
       (class slot &optional no-defaults )
  (values
   (find-ocml-options-value class slot :value)
   (unless no-defaults
     (find-ocml-options-value  class slot :default-value))))





;;;GET-DEFAULT-SLOT-VALUES-FROM-CLASS
(defun get-default-slot-values-from-class (class slot)
  (find-option-value class slot :default-value))


(defun link-binary-argument-types-to-class (name relation list)
  (when list
    (let ((class (get-domain-class (first list))))
      (if class
        (pushnew (cons name relation) (virtual-slots class))
        (ocml-warn "can't link relation ~s to class ~s.  ~s has not been defined"
                   name (first list) (first list))))))

(defun remove-argument-type-link-to-class (name relation list)
  (let ((class (get-domain-class (first list))))
      (when class
        (setf  (virtual-slots class) (remove (cons name relation) (virtual-slots class))))))

(defun current-virtual-slots (class)
 (mapcar #'car 
         (filter  (virtual-slots  class)
           #'(lambda (pair)
               (eq (cdr pair)
                        (get-relation (name (cdr pair))))))))

(defun current-virtual-slots* (class)
  (remove-duplicates 
   (append (current-virtual-slots class)
          (let ((ordered-supers (domain-superclasses class)))
            (loop for class in ordered-supers
                  appending  (current-virtual-slots class)
                  into result
                  finally (return result))))))

(defun type-of-current-virtual-slot (class slot)
  (second (argument-types (cdr (find (cons slot (get-relation slot)) 
                                     (virtual-slots  class) :test #'equal)))))
        

;;;*************************************************************************************

(defun all-ocml-classes (&optional fetch-structures?)
  (if fetch-structures?
    (map-over-hash-table #'(lambda (name structure)
                             name ;;ignore
                             structure
                             )
                         *domain-classes*)
    
    (map-over-hash-table #'(lambda (name structure)
                             structure ;;ignore
                             name)
                         *domain-classes*)))

(defun generate-internal-ocml-class-name (name)
  (gentemp (string name)))

;;;GET-OCML-CLASS
(defun get-ocml-class (Class)
  (gethash class *domain-classes*))
  ;(and (gethash class *domain-classes*)
;       (find-class class)))

;;;GET-DOMAIN-CLASS ---The same as get-ocml-class.  This function will
;;;disappear once I make sure it is not used anywhere.
(defun get-domain-class (class)
  (gethash class *domain-classes*))
  ;(and (gethash class *domain-classes*)
;       (find-class class)))

(defun domain-class? (class-structure)
  (typep class-structure 'ocml-metaclass))

  
(defun remove-domain-class (class)
  (when (get-domain-class class)
    (remhash class *domain-classes*)
    ;;;;;(remove-class class)
    ))

(defun add-domain-class (name class)
  
  (setf (gethash name *domain-classes*)
        class))

;(defun add-domain-class (class)
;  (setf (gethash class *domain-classes*)
;        t))

(defun remove-all-classes ()
  ;(maphash #'(lambda (class ignore)
;               ignore
;               (remove-class class))
;           *domain-classes*)
  (clrhash  *domain-classes*))

(defun rename-class (old-name new-name)
  (let ((class (get-ocml-class old-name)))
    (if class
      (let ((relation (get-relation old-name)))
        (if (can-rename-relation? relation)
          (let ((internal-name (internal-name class)))
            (when (eq internal-name old-name)
              (ocml-warn 
               (string-append 
                (format nil "Renaming class ~s whose lisp name is the same as its OCML name"
                        old-name)
                "...hope you know what you are doing...")))
            (rename-class-internal class old-name new-name )
            (rename-relation-internal relation old-name new-name))
          (error "Can't rename class ~s without messing up the associated unary relation"
                 old-name)))
      (error "Class ~s does not exist" old-name))))

(defun rename-class-internal (class old-name new-name)
  (Let ((virtual-slots (virtual-slots class)))
    (when virtual-slots
      (ocml-warn "Renaming class ~s to ~s means that the first argument type for relations ~{~s ~} is now wrong"
                 old-name new-name virtual-slots))
  (remove-class-in-all-ontologies old-name class nil)
  (setf (name class) new-name)
 ;;;;;;; (remove-domain-class old-name)
  (add-domain-class new-name class)
  (propagate-new-def-to-sub-ontologies new-name class 'class)))

;;;BASIC-DOMAIN-CLASS
(defclass basic-domain-class (name-mixin  basic-ocml-object)
  ((overridden-by :initform nil :accessor instance-overridden-by)
   ))

  
;;;INITIALIZE-INSTANCE :AFTER BASIC-DOMAIN-CLASS
(defmethod initialize-instance :after ((instance basic-domain-class) &rest initargs)
  (declare (ignore initargs))
  (with-slots (name) instance
    (add-instance-to-class (parent-class instance) instance name)))

(defmethod print-object ((instance basic-domain-class) stream)
  (with-slots (name)instance
    (format stream "#<~S ~S>" (name (parent-class instance)) name)))



;;;DEFINE-DOMAIN-CLASS --- This is the entry point for class definition
(defun define-domain-class (name superclasses instance-var  documentation
                                       class-slots relation-spec)
  (multiple-value-bind (instance-var1  documentation1 class-slots1 relation-spec1)
      (parse-class-form instance-var  documentation class-slots relation-spec)
    (check-no-duplicates-in-rel-options
     name relation-spec1 +all-class-definition-legal-options+ 'class)
    (do-class-definition name superclasses instance-var1  documentation1
                         class-slots1 relation-spec1)))
    


;;;CREATE-INITIAL-CLASS-DEFINITION ---This is called to create an initial definition of the class.
;;;The reason why I need to do this is that I need to have the class in place before I can
;;;call finalize-class-spec.
(defun create-initial-class-definition (internal-name superclasses slots lisp-slots) 
                                                      ;;;;;redefinition?)
  (setf superclasses (mapcar #'(lambda (super)
                                 (internal-name (get-ocml-class super)))
                             superclasses))
  ;;(when  (and (not redefinition?)
  ;;            (find-class internal-name nil))
  ;;  (clear-subclasses-slot (find-class internal-name))
  ;;  (unless redefinition?
   ;;   (initialize-instances (find-class internal-name))))
  (eval `(defclass ,internal-name ,superclasses ,(if lisp-slots
                                          (append slots lisp-slots)
                                          slots)
           (:metaclass ocml-metaclass))))

;;;DO-CLASS-DEFINITION
(defun do-class-definition (name superclasses instance-var  documentation    
				 class-slots relation-spec
                                 &aux supers lost-slots new-class slots-to-recompute domain-slots
                                 )
  (let* ((relation-class (get-relation name))
         (local-slots-spec (parse-class-slots name class-slots))
         (local-slots (mapcar #'car local-slots-spec)))
    (when relation-class
      (unless (= (arity relation-class)1)
        (error "Cannot introduce class ~s. A relation named ~s, with arity different from 1 (~a) already exists"
               name name (arity relation-class))))
    (loop for slot in local-slots
          for relation = (get-relation slot)
          do
          (when relation
            (unless  (= (arity relation)2)
              (error "Cannot introduce class ~s with slot ~s. A relation named ~s, with arity different from 2 (~a) already exists"
                     name slot slot (arity relation)))))
    (setf supers (mapcar #'get-ocml-class superclasses))
    (when (member nil supers)
      (error "Some class in ~S has not been defined..when parsing class ~S" superclasses name))
    
    ;;;now every class inherits from the *ocml-top-class*, unless this 
    ;;;does not exists - e.g., because this user has opted out of the base ontology
    (unless supers
      (when (and (get-ocml-class *ocml-top-class*)
                 (not (eq name *ocml-top-class*)))
        (setf supers (list (get-ocml-class *ocml-top-class*)))))
      
    (setf supers  (standardise-superclasses supers)
          superclasses (mapcar #'name supers))
    (Let* ((inherited-slots (set-difference (class-inherited-slots supers)
                                            local-slots))
           (class (get-domain-class name))
	   (already-exists? class)
           (lisp-slots (init-arg-value :lisp-slots relation-spec))
           (internal-name (or (init-arg-value :lisp-class-name relation-spec)
                              (generate-internal-ocml-class-name name)))
           (renaming-pairs (second (member :slot-renaming relation-spec)))
           (ordered-supers) (inherited-chains))
      
      (when renaming-pairs 
        (setf relation-spec (remove-if #'(lambda (x) (or (eq x :slot-renaming)
                                                         (equal x renaming-pairs)))
                                       relation-spec)
             )) ;;the two bits below should be done anyway...I hope...26/10/01
      (setf
     
              inherited-chains (normalize-inherited-chains    ;;removes redundant chains
                                (mapcar #'renaming-chains 
                                        supers))
              renaming-pairs (check-local-renaming-feasibility ;;checks validity of new renaming pairs
                              name local-slots 
                              inherited-slots renaming-pairs
                              inherited-chains))
      ;)
      (let ((class-with-same-internal-name (find-class internal-name nil)))
        (when class-with-same-internal-name 
          ;;a class with the same internal name already exists. we need to make sure that no conflicts arise.
          (cond ((not (typep class-with-same-internal-name 'ocml-metaclass))
                 (ocml-warn "cannot use internal name ~s for class ~s..a lisp class with the same name already exists.
A different internal name will be generated..."
                            internal-name name)
                 (setf internal-name (generate-internal-ocml-class-name name)))
                (t
                 (let (in-use?
                       (lisp-class-ontology (home-ontology class-with-same-internal-name))
                       (current-ontology *current-ontology*))
                   (when (and lisp-class-ontology
                              (eq (get-ontology  ;;we check that this is an active ontology
                                   (name lisp-class-ontology))
                                  lisp-class-ontology))
                     (unwind-protect (progn
                                       (switch-to-ontology lisp-class-ontology)
                                       (setf in-use? (get-domain-class 
                                                      (name class-with-same-internal-name))))
                       (switch-to-ontology current-ontology))
                     (when in-use?
                       (unless 
                         ;;only if we are re-defining the same class we can ignore the internal name conflict
                         (and (eq (name class-with-same-internal-name) name)
                              (eq lisp-class-ontology *current-ontology*))
                         (ocml-warn "cannot use internal name ~s for class ~s..OCML class ~s in ontology ~s already uses this internal name.  
A different internal name will be generated..."
                                    internal-name name (name class-with-same-internal-name) (name lisp-class-ontology))
                         (setf internal-name (generate-internal-ocml-class-name name))))))))))
      
      (cond ((and already-exists?
                  (eq (home-ontology class) *current-ontology*))
             (warn "Redefining class ~S" name)
             (setf (find-class internal-name)  ;;Hope this works in all common lisp systems.....
                   class) 
             (setf lost-slots (set-difference (domain-slots class) 
                                              (append local-slots inherited-slots)))
             (remove-existing-slot-renaming-pairs class))
            (already-exists?
             ;;class already exists in a super-ontology..this definition will override the previous
             ;;one.  In practice it is as if the previous one does not exist
             (setf already-exists? nil))
            ((find-class internal-name nil)
             ;;;(and (find-class internal-name nil)
             ;;;     (typep (find-class internal-name)'ocml-metaclass))
             
             (clear-subclasses-slot (find-class internal-name))
             (initialize-instances (find-class internal-name))
             (setf (home-ontology (find-class internal-name))
                   *current-ontology*)))
      (setf class (create-initial-class-definition 
                   internal-name superclasses local-slots lisp-slots)) 
      #+:allegro (clos:finalize-inheritance class)
      #+:sbcl (sb-mop:finalize-inheritance class)
      ;;the next lines have to do with slot renaming in classes --16/2/99
      
      (setf 
       (renaming-pairs class)  renaming-pairs
       (renaming-chains class) (construct-renaming-chains-for-class 
                                renaming-pairs
                                inherited-chains)
       (effective-renaming-chains class) (merge-renaming-chains (renaming-chains class))
       
       ;;let's save the original local slots spec before any renaming-related changes
       (ocml-options-no-renaming class) local-slots-spec)
      
      (setf
       ;;here we extend the local slots spec to include all renaming as well
       local-slots-spec (standardize-local-slots-spec-for-renaming  
                         local-slots-spec
                         (effective-renaming-chains class))
       ;;finally, let's update local and inherited to reflect the above change
       local-slots (mapcar #'car local-slots-spec)
       inherited-slots (set-difference inherited-slots local-slots)
       ordered-supers (sort supers #'(lambda (x y)
                                       (< (position x (class-precedence-list class))
                                          (position y (class-precedence-list class))))))
      (multiple-value-bind (clos-specs slot-info-list ocml-options)
                           (finalize-class-spec  
                            name
                            ordered-supers
                            local-slots-spec
                            inherited-slots
                            (effective-renaming-chains class))
        ;;here we ensure that all slots-info both local and 
        ;;inherited takes renaming into account
        ;;(setf slot-info-list
        ;;      (standardize-slots-spec-for-renaming  class ordered-supers slot-info-list ocml-options))
        
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
            (remove-slot-of-entry (get-relation slot) class)
            (remove-local-slot-of-entry (get-relation slot) class))
          (Setf domain-slots (domain-slots new-class)
                slots-to-recompute                   ;;The slots to recompute are all the domain slots
                (union (domain-slots new-class)      ;;of the new class, + the slots 
		       lost-slots))                  ;;which have been 'lost'
       	  (update-class-direct-instances new-class
                                         domain-slots) ;;Only domain slots are needed for instances 
	  (update-subclasses new-class slots-to-recompute  lost-slots))
        ;;;harlequin can't quite cope
        #+lispworks
        (ensure-all-superclasses-know-me new-class supers)
        (propagate-new-def-to-sub-ontologies name new-class 'class)
        new-class))))

(defun standardise-superclasses (supers)
  (loop with result = nil
        for super in supers
        for test =  (loop  for super2 in (remove super supers)
                           for flag = (subclass-of? super2 super)
                           until flag
                           finally
                           (return flag))
        do
        (unless test
          (push super result))
        finally
        (return result)))
       
;;;STANDARDIZE-LOCAL-SLOTS-SPEC-FOR-RENAMING
;;;Uses teh effective-renaming-chains to propagate local slot spec through the 
;;;renaming chains
(defun standardize-local-slots-spec-for-renaming (slots-spec chains)
  
  (loop 
    with new-slots-spec = nil
    for one-spec in slots-spec
    for slot = (first one-spec)
    for renamed-slots = (all-slots-renamed-by-slot-x slot chains)
    do
    (loop for rslot in  renamed-slots
          do
          (push (cons rslot (cdr one-spec)) new-slots-spec))
    finally
    (return (append slots-spec new-slots-spec))))

;(defun standardize-slots-spec-for-renaming (class slot-info-list &optional partial-slot-info-list?)
;    (loop with chains = (renaming-chains class)
;          with result = slot-info-list
;          for chain in chains
;          do
;          (setf result  (standardize-slots-spec-for-chain  result chain partial-slot-info-list?))
;          finally
;          (return result)))

;(defun standardize-slots-spec-for-renaming (class ordered-supers slot-info-list ocml-options
;                                                   &optional partial-slot-info-list?)
;    (loop with chains = (renaming-chains class)
;          with result = slot-info-list
;          for chain in chains
;          do
;          (setf result  (standardize-slots-spec-for-chain ordered-supers  result chain ocml-options
;                                                          partial-slot-info-list?))
;          finally
;          (return result)))

;(defun standardize-slots-spec-for-chain ( slot-info-list chain partial-slot-info-list?)
;  (let* ((structures (mapcar #'(lambda (slot)
;                                 (right-value slot slot-info-list))
;                             chain))
;         (result))
;    (unless (and partial-slot-info-list?
;                 (null (filter structures #'identity)))
;      (setf result (merge-slot-info-structures structures))
;      (loop for slot in chain
;            do
;            (setf slot-info-list
;                  (substitute (cons slot result)
;                              (assoc slot slot-info-list)
;                              slot-info-list))))
;    slot-info-list))
                                
   
;(defun standardize-slots-spec-for-chain ( precedence-list slot-info-list chain ocml-options 
;                                                           partial-slot-info-list? )
;  (let* ((structures (mapcar #'(lambda (slot)
;                                 (right-value slot slot-info-list))
;                             chain))
;         (result))
;    (unless (and partial-slot-info-list?
;                 (null (filter structures #'identity)))
;      (setf result (merge-slot-info-structures precedence-list  chain  structures ocml-options))
;      (loop for slot in chain
;            do
;            (setf slot-info-list
;                  (substitute (cons slot result)
;                              (assoc slot slot-info-list)
;                              slot-info-list))))
;    slot-info-list))

(defun ensure-all-superclasses-know-me (class supers)
  (loop for super in supers
        do (unless (member class (subclasses super))
             (setf (direct-subclasses super) (cons class (direct-subclasses super))))))

;;;MAKE-DOMAIN-CLASS
(defun make-domain-class (name internal-name supers  instance-var documentation 
                               slots-spec       ;;clos-like spec
                               domain-slots     ;;All domain slots (local and inherited)
                               local-slots
                               lisp-slots
                               ocml-options
                               ocml-slot-info   ;;local + inherited ocml options
			       relation-spec
                               ;;;;redefinition?
                               &aux class own-slots)
  (unless documentation
    (setf documentation ""))    ;;Needed for maclisp--Enrico
  
  (prog1
      (setf class
	    (eval `(defclass ,internal-name ,(ensure-vanilla-class2
		                              supers 
		                              'basic-domain-class)
           , (if lisp-slots 
               (append slots-spec lisp-slots)
               slots-spec)
           ,(list :documentation documentation)
           ,(list :metaclass 'ocml-metaclass))))
 #-:lispworks(record-source-file name 'ocml-class)
    ;;#+(or allegro lispworks)(record-source-file name 'def-class)
    #+(or allegro lispworks)(ocml-record-source-file name 'def-class)
    (add-domain-class name class)
    (setf own-slots (init-arg-value :own-slots relation-spec))
    (set-metaclass-information class domain-slots ocml-slot-info
			        local-slots ocml-options 
                                own-slots
                                name instance-var)
    (setf relation-spec (remove-if #'(lambda (x)
                                       (or (eq x :own-slots)
                                           (and own-slots
                                                (eq x own-slots))))
                                   relation-spec))
    (add-rules-associated-with-slot-renaming name)
    (ensure-class-is-relation name instance-var relation-spec)
    (add-slot-of-links class)))


;;;COMPUTE-SLOTS-TO-RECOMPUTE
;;;This function returns the list of slots which need to be recomputed after
;;;a class redefinition.  This contains the old and the new local slots of the class,
;;;+ all the slots inherited from old superclasses (which are not supers anymore)
;;;and those inherited from new supers (which were not supers before).
;;;NOT USED
;(defun compute-slots-to-recompute (class new-supers new-local-slots)
;  (union (local-slots class)
;         (union new-local-slots
;                (class-inherited-slots
;                 (set-exclusive-or new-supers
;                                   (direct-domain-superclasses  class))))))

(defun update-subclasses (class slots-to-recompute lost-slots);;;; new-supers lost-supers)
  (loop 
        with subclasses = (remove-duplicates         ;;;Get all subclasses (including the indirect ones)
                           (subclasses class))
	while subclasses
        for subclass = (pick-next-subclass subclasses)
        do
	(redefine-subclass-after-class-update subclass slots-to-recompute lost-slots) 
	(setf subclasses (remove subclass subclasses))))

(defun pick-next-subclass (subclasses)
  (find nil subclasses :test #'(lambda (x y)
                                 (declare (ignore x))
                                 (null (intersection
                                        (direct-superclasses nil y)
                                        subclasses)))))
        

;(defun add-slots-from-renaming-chains (slots-to-recompute chains)
;  (loop with additional-slots = nil
;        for slot in slots-to-recompute
;        do
;        (setf additional-slots (union additional-slots
;                                      (collect-all-associated-slots-in-renaming-chains slot chains)))
;        finally 
;        (return (union additional-slots slots-to-recompute))))
  
(defun add-slots-from-renaming-chains (slots-to-recompute effective-chains)
  (loop for chain in effective-chains
        do
        (when (intersection slots-to-recompute chain)
          (setf slots-to-recompute
                (union slots-to-recompute chain))))
  slots-to-recompute)


(defmethod redefine-subclass-after-class-update ((class ocml-metaclass)
                                                 slots-to-recompute
                                                 lost-slots) ;;;; new-supers lost-supers) 

  ;;slots-to-recompute = (domain-slots of redefined-superclass + lost-slots)
  ;;first we add to slots-to-recompute the slots which rename inherited slots. 
  ;; These renaming slots can derive either from local renaming pairs or from 
  ;;renaming chains inherited from other superclasses

  (with-slots (effective-renaming-chains renaming-chains renaming-pairs slot-info-alist local-slots 
                               ocml-options-no-renaming ocml-options) 
              class
    
    
    (Let* ((superclasses (direct-domain-superclasses  class))
           (domain-slots-from-supers (class-inherited-slots superclasses))
           (real-local-slots (mapcar #'car ocml-options-no-renaming))
           (domain-slots (union domain-slots-from-supers  ;;domain slots from supers + locally defined slots
                                real-local-slots))
           (new-slots (set-difference domain-slots (domain-slots class)))
           (inherited-chains (normalize-inherited-chains (mapcar #'renaming-chains superclasses))))
      
      (setf lost-slots (set-difference lost-slots domain-slots))
      ;;let's update renaming info
      (setf slots-to-recompute 
            (add-slots-from-renaming-chains 
             slots-to-recompute effective-renaming-chains))
      (setf 
       renaming-pairs (check-local-renaming-feasibility 
                       (name class) real-local-slots (set-difference domain-slots real-local-slots)
                       renaming-pairs inherited-chains)
       renaming-chains (construct-renaming-chains-for-class
                        (renaming-pairs class)
                        inherited-chains)
       effective-renaming-chains (merge-renaming-chains renaming-chains)
       slots-to-recompute (add-slots-from-renaming-chains ;;we do this again because new slots 
                           slots-to-recompute             ;;may have been added...
                           effective-renaming-chains)) 
      
      (let*  ((relevant-local-slots) (other-relevant-slots) 
              (new-slot-info-list)
              (ordered-supers (sort superclasses #'(lambda (x y)
                                     (< (position x (class-precedence-list class))
                                        (position y (class-precedence-list class)))))))
        (setf ocml-options
              ;;here we make sure that all local slot specifications
              ;;are consistent with renaming
              (standardize-local-slots-spec-for-renaming ocml-options-no-renaming
                                                         effective-renaming-chains))
        
        ;;now, we need to update local-slots and related variables
        (setf local-slots (mapcar #'car ocml-options)
              relevant-local-slots (intersection local-slots slots-to-recompute)
              other-relevant-slots (intersection  ;;makes sure we do not deal with foreign slots
                                    (set-difference slots-to-recompute relevant-local-slots)
                                    domain-slots-from-supers))
        
        (loop for slot in relevant-local-slots
              for slot-info = (calculate-slot-info 
                               (name class)
                               ordered-supers
                               slot
                               (right-value* slot ocml-options)
                               (find-renaming-chain-with-slot
                                slot effective-renaming-chains))
              do
              (push (cons slot slot-info) new-slot-info-list))
        
        (loop for slot in other-relevant-slots
              for slot-info = (calculate-slot-info 
                               (name class)
                               ordered-supers
                               slot
                               nil                   ;;;Of course there are no local ocml options
                               (find-renaming-chain-with-slot
                                slot effective-renaming-chains))    
              do
              (push (cons slot slot-info) new-slot-info-list))
        
        ;;now we add renaming-derived info to the slot-info-list 
       ;; (setf new-slot-info-list
       ;;       (standardize-slots-spec-for-renaming  class class-precedence-list new-slot-info-list
       ;;                                              ocml-options t))
        
        ;;finally, let's makes sure that also teh slots we did not recompute are part 
        ;;of the new slot-info-alist
        (loop for pair in slot-info-alist
              do
              (unless (or (assoc (car pair)            ;unless it is already in new-slot-info-list
                                 new-slot-info-list)
                          (not (member (car pair)      ;or it is not inherited from a superclass anymore
                                       domain-slots)))
                          
                (push pair new-slot-info-list)))
        
        
        
        
        (when lost-slots
          (dolist (slot lost-slots) ;;;;;(set-difference lost-slots (domain-slots class)))
            (remove-slot-of-entry (get-relation slot) class)
            ;;;lost slots cannot be local
            ;;;;(remove-local-slot-of-entry  
            ;;;;   (get-relation slot) class)
            ))

        (when new-slots
          (dolist (slot new-slots) ;;;;;(set-difference lost-slots (domain-slots class)))
            (add-slot-of-entry (get-relation slot) class)
            (when (member slot local-slots)
              (add-local-slot-of-entry (get-relation slot) class))))
              

        (set-metaclass-information class domain-slots
                                   new-slot-info-list local-slots ocml-options)
        (update-class-direct-instances class (append relevant-local-slots other-relevant-slots))))))


(defmethod update-class-direct-instances ((class ocml-metaclass)slots-to-recompute)
                                             ;;;;;new-supers lost-supers)
  (if (eq *current-ontology* (home-ontology class))
    (update-class-direct-instances-internal class slots-to-recompute)
    (let ((current *current-ontology*))
      (unwind-protect 
        (progn
          (switch-to-ontology (home-ontology class))
          (update-class-direct-instances-internal class slots-to-recompute))
        (unless (eq *current-ontology* current)
          (switch-to-ontology current))))))

(defun update-class-direct-instances-internal (class  slots-to-recompute)
          
  (with-slots (direct-instances-list) class
    (loop for instance  in direct-instances-list ;;;;being the hash-value in instances
          do
          ;(update-fc-instance-of-links             ;;;Deleted, as we now only link objects to fc rules
;           (name instance) new-supers lost-supers) ;;;at compile-ruleset time
          (update-instance-after-class-redefinition instance class slots-to-recompute))))

         

;;;FINALIZE-CLASS-SPEC --- It does the following:
;;;1. It computes the initform of the class.  This
;;;will comprise the value of the associated slot.  This value might be
;;;inherited from superclasses
;;;2. It creates a slot-info structure which includes all the ocml options
;;;either local or  inherited from superclasses
;;;3. It records all local ocml options associated with each slot
;;;
(defun finalize-class-spec (class-name ordered-supers local-slot-specs inherited-slots chains)
  ;;inherited-slots is a list of non-local slots - e.g. (slot-1 slot-2)
  (loop with result = nil
        with slot-info-list = nil
        with ocml-options-record = nil
	for slot-spec in local-slot-specs
        ;;each slot-spec is a list of 1 to 3 elements
        do
	(destructuring-bind (slot &optional ocml-options clos-options)
	    slot-spec
          (let ((slot-info 
                 (calculate-slot-info 
	          class-name
	          ordered-supers
	          slot ocml-options
                  (find-renaming-chain-with-slot slot chains))))
            (push 
	     `(,slot
	       :initarg ,slot 
	       ,@(apply #'append clos-options))
             result)
            (push (cons slot slot-info) slot-info-list)
            (push (cons slot ocml-options)ocml-options-record)))
        finally
        (Loop for slot in inherited-slots
             for slot-info = (calculate-slot-info 
                              class-name
	                      ordered-supers
	                      slot nil
                              (find-renaming-chain-with-slot slot chains))
             do
             (push (cons slot slot-info) slot-info-list))
        (return (values result slot-info-list ocml-options-record))))



 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;STRUCTURE SLOT-INFO
(defstruct (slot-info (:conc-name ocml-slot-))
  (min-cardinality)
  (max-cardinality)
  (value)
  (default-value)
  (inheritance)
  (type-options)
  (type))

(defun get-slot-info-from-structure (option structure)
  (let ((*package* (find-package "OCML"))) 
    (funcall (read-from-string (string-append "OCML-SLOT-" (string option)))
             structure)))

;;;New version which takes into account renaming - enrico 17/3/99
(defun calculate-slot-info (class-name ordered-supers slot ocml-options chain)
  (unless chain
    (setf chain (list slot)))
  (let* ((inheritance (decide-inheritance-type ordered-supers chain ocml-options))
         (type-options (decide-type-option class-name ordered-supers chain ocml-options))
         (values (decide-value-options ordered-supers chain ocml-options
                                       (or inheritance *default-inheritance* )))
         (min-cardinality (decide-min-cardinality ordered-supers chain ocml-options ))
         (max-cardinality (decide-max-cardinality ordered-supers chain ocml-options)))
    (make-slot-info :inheritance inheritance
		    :value (car values)
		    :default-value (second values)
		    :min-cardinality min-cardinality
		    :max-cardinality max-cardinality
		    :type-options type-options)))

(defun decide-inheritance-type (ordered-supers chain ocml-options)
  (find-first-option-value* ordered-supers chain :inheritance ocml-options ))

(defun decide-value-options (ordered-supers chain ocml-options inheritance)
  "Returns a list of two elements.  The first contains all
definitional slot values for this slot, and the second all the
relevant slot values"
  (list
   (remove-duplicates (find-all-option-values*  ordered-supers chain :value ocml-options)
                      :test #'equal)
   (case inheritance
      (:supersede
       (find-default-values ordered-supers chain ocml-options))
      (:merge
        (remove-duplicates (find-all-option-values*  ordered-supers chain 
                                                     :default-value ocml-options)
                           :test #'equal)))))

(defun find-default-values (ordered-supers chain ocml-options &aux local-values)
  ;;;each option ((option value) .... (option value))
  (mapc #'(lambda (pair)
            (when (eq (car pair) :default-value)
              (Push (second pair) local-values)))
        ocml-options)
  (or local-values
        (loop for class in ordered-supers
              for result1 = (loop for slot in chain
                                  for result2 = (find-option-value class slot :default-value)
                                  until result2
                                  finally (return result2))
            until result1
            finally (return result1))))

(defun decide-type-option (class-name superclasses chain ocml-options)
  (list class-name superclasses chain ocml-options))

;; XXX FINALIZE-ONTOLOGY is called in the wrong place, IMHO.  It's
;; called at the end of the DEF-ONTOLOGY clause.  Now, that means that
;; interactive entry of an ontology cannot be done, because certain
;; operations (such as recomputing slot type specifiers) can be done
;; only after the ontology's contents have been seen.
(defun finalise-ontology (ontology)
  (with-ontology (ontology)
     (compute-slot-type-specifiers ontology)))

(defun compute-slot-type-specifiers (ontology)
  "Once the ontology is loaded, compute slot types from the slot type specifiers."
  ;; The classes must be ordered such that subclasses are preceded by
  ;; any superclasses, so that inherited slot specifiers are valid
  ;; when the subclass goes looking for them.
  (let ((classes (sort (mapcar #'get-domain-class
                               (list-hash-table (ontology-classes (ontology-directory ontology))))
                       (lambda (a b) (superclass-of* a (list b))))))
    (dolist (class classes)
      (let ((slot-infos (slot-info-alist class)))
	(dolist (slot-info slot-infos)
	  (let ((struct (cdr slot-info)))
	    (setf (ocml-slot-type struct)
		  (apply #'compute-slot-type (ocml-slot-type-options struct)))))))))

(defun compute-slot-type (class-name superclasses chain ocml-options)
  (let* ((new (remove-duplicates (apply #'append
					(mapcar #'(lambda (option)
						    (when (eq (car option) :type)
						      (list(second option))))
						ocml-options))))
	 (inherited (remove-duplicates
		     (apply #'append
			    (mapcar #'(lambda (super)
					(loop for slot in chain
					   for result2 = (find-option-value super slot :type)
					   until result2
					   finally (return result2)))
				    superclasses))))
	 (all-types (union new inherited))
	 (undefined-types (filter all-types #'(lambda (type)
						(and (member type all-types)
						     (not (get-ocml-class type)))))))
    (when (member-if #'(lambda (type)
			 (and (listp type)
			      (eq (car type) 'or)))
		     new)
      (funcall (if (member :irs-ocml-hacks cl:*features*)
		   #'warn
		   #'error)
	       "Slot ~a of class ~a has an invalid type specification: OR type specifications are not allowed"
	       (car chain) class-name))
    (cond (undefined-types
	   ;; XXX This should be an error, but some ontologies depend on it.
	   (ocml-warn "Class ~A references undefined classes ~A in definition of slot ~A."
		      class-name undefined-types (car chain))
	   nil)
	  (t (mapcar #'name (remove-subsuming-classes (mapcar #'get-domain-class all-types)))))))

(defun decide-min-cardinality (superclasses chain ocml-options)
  (let ((cardinalities (find-all-option-values*
                        superclasses chain :min-cardinality
                        (mapcar #'(lambda (pair)
                                    (if (eq (car pair) :cardinality)
					(list :min-cardinality (second pair))
					pair))
                                ocml-options)
                        t)))
    (when cardinalities
      (apply #'min cardinalities))))

(defun decide-max-cardinality (superclasses chain ocml-options )
  (let ((cardinalities (find-all-option-values*
                        superclasses chain :max-cardinality 
                        (mapcar #'(lambda (pair)
                                    (if (eq (car pair) :cardinality)
					(list :max-cardinality (second pair))
					pair))
                                ocml-options)
                        t)))
    (when cardinalities
      (apply #'max cardinalities))))

(defun find-first-option-value* (ordered-supers chain option ocml-options)
  ;;;each option ((option value) .... (option value))
  (let ((value (right-value* option ocml-options)))
    (or value
        (loop for class in ordered-supers
              for result1 = (loop for slot in chain
                                  for result2 = (find-option-value class slot option)
                                  until result2
                                  finally (return result2))
            until result1
            finally (return result1)))))

(defun find-all-option-values* (ordered-supers chain option ocml-options &optional collect?
                                               &aux values)
  ;;;cardinality options are stored as atoms, rather than as lists.  This means that
  ;;;we have to collect them, rather than appending
  
  ;;;each option = ((option value) (option value))
  (let ((fun (if collect?
               #'(lambda (local-value list)
                   (push local-value list))
               #'append)))
    (mapc #'(lambda (pair)
              (when (eq (car pair) option)
                (Push (second pair) values)))
          ocml-options)
    (loop for class in ordered-supers
          do
          (loop for slot in chain
                for class-values = (find-option-value class slot option)
                do
                (when class-values 
                  (setf values (funcall fun  class-values values)))))
    values))
