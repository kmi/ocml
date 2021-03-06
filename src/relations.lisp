;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defun add-to-relations-directory (name instance)
  (setf (gethash name *defined-relations*) instance))

(defun remove-all-relations ()
  (clrhash *defined-relations*))

(defun get-relation (name)
  (gethash name *defined-relations*))

(defun get-relation-from-ontology (name ontology)
  (gethash name (ontology-relations (ontology-directory ontology))))

(defun remove-relation-internal (name)
  (remhash name *defined-relations*))

(defun all-relations (&optional structures)
  "List names of all relations, or the structures if STRUCTURES is
non-nil."
  (map-over-hash-table (if structures
                           (lambda (name structure)
                             (declare (ignore name))
                             structure)
                           (lambda (name structure)
                             (declare (ignore structure))
                             name))
                       *defined-relations*))

;;;RELATION 
(defclass ocml-relation (name-mixin lisp-attachment-mixin ;;;;;onto-spec-mixin
                                    basic-ocml-object)
  ((arity :initarg :arity :initform nil :accessor arity)
   (schema :initarg :schema :initform nil :accessor schema )
   (defined-from-def-relation :initarg :defined-from-def-relation 
     :initform nil :accessor defined-from-def-relation  )
   
   ;;argument-types should not be used anymore
   (argument-types :initform nil :initarg :argument-types :accessor argument-types)

   (defined-from-class? :initform nil :initarg :defined-from-class? :accessor defined-from-class?)
   (constraint :initarg :constraint :initform nil :accessor constraint)
   ;;;(def :initarg :def :initform nil)
   (sufficient :initarg :sufficient  :initform nil :accessor sufficient)
   (iff-def :initarg :iff-def :initform nil :accessor iff-def)
   (own-slot-of  :accessor own-slot-of :initform nil)
   (prove-by  :initarg :prove-by :initform nil :accessor prove-by)
   (exclusive-prove-by  :initarg :exclusive-prove-by :initform nil 
                        :accessor exclusive-prove-by)
   (no-proofs-by :initarg :no-proofs-by :initform nil :accessor no-proofs-by)
   (avoid-infinite-loop :initarg :avoid-infinite-loop :initform nil :accessor avoid-infinite-loop)
   (sufficient-for-type-checking :initarg :sufficient-for-type-checking  
                                 :initform nil 
                                 :accessor sufficient-for-type-checking)
   (no-op :initarg :no-op :initform nil)
  ;;;;; (axiom-def :initarg :axiom-def  :initform nil)
   (relation-instances :initform nil :accessor relation-instances)
   (upward-mapping? :initform nil :accessor upward-mapping?)
   (downward-add-exp :initform nil
		     :accessor downward-add-exp)
   (downward-remove-exp :initform nil
                        :accessor downward-remove-exp)
   
   (slot-of :accessor slot-of       ;This is a list of classes which have a local or inherited slot
            :initform nil)          ;with the same name as this relation
   (local-slot-of :accessor local-slot-of :initform nil)
   (defined-by-rule :initform nil
                    :accessor defined-by-rule)
   (cache-values? :initarg :cache-values? :initform nil
                  :accessor cache-values?)            ;Cache the queries if t
   (cache-table :initform nil :accessor relation-cache-table) 
   (fc-nodes :initform nil :accessor fc-nodes )
   (indirect-fc-nodes :initform nil
                      ;;This entry only makes sense if the relation is a slot. If this is the case,
                      ;;say relation is sloti, then this entry contains a reference to all alpha nodes
                      ;;such as (<class> <x> .........<sloti> <value>....)
                      :accessor indirect-fc-nodes)
   (lisp-slots        ;;;The name of the lisp class implementing  an
    :initarg :lisp-slots)
   (lisp-class-name             ;;;The name of the lisp class implementing  an
    :initarg :lisp-class-name)) ;;;OCML class.  Only used to avoid parsing the relation spec
  (:documentation "The class of relations in OCML"))

;;; def-relation-instances-internal calls ocml-record-source-file and
;;; CCL refuses to accept it unless the type is defined.  So we define
;;; it here just to have something to talk about.
(defclass ocml-relation-instances
    (name-mixin lisp-attachment-mixin basic-ocml-object)
  ())

(defun schema? (thing)
  (and (listp thing)
       (every #'variable? thing)
       (equal (remove-duplicates thing) thing)))

(defun kappa-exp? (thing)
  (and (listp thing)
       (eq (car thing) 'kappa)
       (schema? (second thing))
       (listp (third thing))))

;(defmacro define-relation-internal (name schema documentation &rest options)
 ; (multiple-value-bind (name schema documentation options)
;      (parse-define-relation-form name schema documentation options)
;   `(funcall #'make-ocml-relation ',name :schema ',schema :documentation  ,documentation
;	      ,@(mapcar #'(lambda (x)
;                            (list 'quote x))
;			options))))

(defun define-relation-internal (name schema documentation options)
  (multiple-value-bind (name schema documentation options)
      (parse-define-relation-form name schema documentation options)
    (check-no-duplicates-in-rel-options name options)
    (prog1
        (apply #'make-ocml-relation name :schema schema :documentation
               documentation :defined-from-def-relation t options)
      (ocml-record-source-file name 'ocml-relation))))

(defun check-no-duplicates-in-rel-options
    (name spec &optional (option-list +relation-spec-keywords+) (type 'relation))
  (loop with duplicates
        for keyword in option-list
        for n = (count keyword spec)
        do
        (when (> n 1)
          (push keyword duplicates))
        finally
        (when duplicates
          (if (cdr duplicates)
            (warn "More than one occurrence of the following keywords was found when parsing ~(~s~) ~s:~% <~{~s, ~}~s>.  Only the first occurrence will be considered."
                type name 
                (butlast duplicates)
                (car (last duplicates)))
            (warn "The ~s keyword appears more than once in ~(~s~) ~s. Only the first occurrence will be considered."
                (car duplicates)
                type
                name)))))

       
(defun rename-relation (old-name new-name)
  (let ((relation (get-relation old-name)))
    (with-slots (relation-instances  sufficient iff-def
                 upward-mapping? downward-add-exp downward-remove-exp 
                 home-ontology slot-of defined-by-rule fc-nodes)
                relation
      (if relation
        (if (can-rename-relation? relation)
          (let ((class (get-ocml-class old-name)))
            (if class
              (ocml-warn 
               "Can't rename a relation associated with a class...the class needs to be renamed first")
              (rename-relation-internal relation old-name new-name)))
          (cond 
           ((not (eq home-ontology *current-ontology*))
             (ocml-warn "Relation ~s has been defined in ontology ~s, which is different from the 
                         currently selected ontology, ~s.  Imported definitions cannot be renamed in 
                         a sub-ontology"
                        old-name home-ontology *current-ontology*))
           (relation-instances
            (ocml-warn "Cannot rename relation ~s: associated facts must be removed first"
                       old-name))
           (sufficient (ocml-warn 
                        (string-append 
                         (format nil "Cannot rename relation ~s, which has a sufficient condition "
                                 old-name)
                         "associated...better to delete and redefine it")))
           (iff-def (ocml-warn 
                        (string-append 
                         (format nil "Cannot rename relation ~s, which has an iff-def condition "
                                 old-name)
                         "associated...better to delete and redefine it")))
           (slot-of
            (ocml-warn 
             (string-append "Cannot rename relation ~s, which is a slot of classes ~s."
                            " These classes need to be deleted first")
             old-name slot-of))
           (defined-by-rule
             (ocml-warn "Cannot rename relation ~s: associated backward rules must be removed first"
                        old-name))
           (fc-nodes
            (ocml-warn "Cannot rename relation ~s: associated forward rules must be removed first"
                       old-name))
           ((or upward-mapping? downward-add-exp downward-remove-exp)
            (ocml-warn "Cannot rename relation ~s without losing the associated mappings"
                       old-name))
           (t
            (error "Internal error"))))
        (error "Relation ~s does not exist" old-name)))))


(defun rename-relation-internal (relation old-name new-name)
  (remove-relation-in-all-ontologies old-name relation)
  ;;;;;(remove-relation-internal old-name)
  (setf (name relation) new-name)
  (when (= (arity relation) 2)
    ;;;;;;(remove-argument-type-link-to-class ;;this bit done by remove-relation-in-all-ontologies
    ;;;;;; old-name (argument-types relation))
    (link-binary-argument-types-to-class new-name relation (argument-types relation)))
  (add-to-relations-directory new-name relation)
  (propagate-new-def-to-sub-ontologies new-name relation 'relation))
  
(defmethod can-rename-relation? ((relation ocml-relation))
  (with-slots (relation-instances  iff-def sufficient
                upward-mapping? own-slot-of downward-add-exp downward-remove-exp 
                slot-of defined-by-rule fc-nodes)
              relation
    (not (or relation-instances  iff-def sufficient
                upward-mapping? downward-add-exp downward-remove-exp 
                slot-of own-slot-of defined-by-rule fc-nodes
                (not (eq (home-ontology relation) *current-ontology*))))))

(defmethod print-object ((obj ocml-relation)stream)
  (with-slots (name) obj
  (format stream "<OCML-RELATION ~S>" name)))

;(defun make-ocml-relation (rel &rest options )
;  (when (get-relation rel)
;    (ocml-warn "Redefining relation ~S" rel))
;  (let ((instance (apply #'make-instance
;                         'ocml-relation
;                         :name rel
;                         options)))
;    (maybe-process-sufficient-&-iff-def-entries instance)
;    (propagate-new-def-to-sub-ontologies rel instance 'relation)
;    instance))




;;;MAKE-OCML-RELATION
(defun make-ocml-relation (rel &rest options )
  (let ((structure (get-relation rel))
       rel-instances 
        classes local-classes rules)
    (when structure
      ;;OK, we have defined a relation which already exists 
      (let ((arity (length (init-arg-value :schema options))))
        (cond ((get-domain-class rel)
               (unless (= arity 1)
                 (error "relation ~s is associated with a class and therefore cannot have arity other than 1"
                        rel)))
              ((slot-of structure)
               (if (= arity 2)
                 (setf classes (slot-of structure)
                       local-classes (local-slot-of structure))
                 (error "relation ~s is a slot of classes ~{~s ~} and cannot be redefined with an arity other than 2"
                        rel (slot-of structure)))))
        
        ;;ok, no incompatibilities with slots or classes
        (let ((old-ontology (home-ontology structure))
                (source-file (car (source-files rel 'ocml-relation))))
            (cond ((eq old-ontology *current-ontology*)
                   (unless (equal (and source-file
                                       (translate-logical-pathname source-file))
                                  (and (current-file)(translate-logical-pathname (current-file))))
                     (if source-file
                       (ocml-warn "Redefining relation ~S, previously defined in file ~a"
                                  rel source-file)
                       (ocml-warn "Relation ~s redefined in file ~a"
                                  rel (current-file)))))
                  (t
                   (ocml-warn 
                    "Redefining relation ~S, previously defined in ontology ~s" rel (name 
                                                                                     old-ontology)))))
                   
                   
                   

        (when (and (eq *current-ontology* (home-ontology structure))
                   (= (arity structure) 2)
                   (argument-types structure))
          ;;if we are really replacing the previous definition, rather than just overriding, then
          ;;we need to remove the argument links
          (remove-argument-type-link-to-class rel structure (argument-types structure)))
        
        (when (= arity (arity structure))
          ;;we can carry through the existing rules... <old comment>

          ;;it seems a good idea to carry thorugh also the rel-instances...but for some reason
          ;;I did not allow it.  But I can't remember why, so let's try again - i.e.
          ;; we allow rel-instances to be carried through!
          (setf rel-instances (relation-instances structure)
           rules (defined-by-rule structure)))))

    (make-ocml-relation-internal rel options rel-instances classes local-classes rules)))


(defun make-ocml-relation-internal (rel options &optional rel-instances classes local-classes rules)
  (check-proofs-by-consistency rel options)
    (let ((instance (apply #'make-instance
                           'ocml-relation
                           :name rel
                           options)))
      (setf (relation-instances instance)
            rel-instances
            (slot-of instance) classes
            (local-slot-of instance) local-classes
            (defined-by-rule instance) rules)
      
     ;;(loop for class in classes
     ;;       do
     ;;       (with-slots (slots-as-relations) class
     ;;        (add-slot-of-entry  instance class)))
            
      (maybe-process-sufficient-&-iff-def-entries instance)
      (link-binary-argument-types-to-class rel instance
       (init-arg-value :argument-types options))
      (propagate-new-def-to-sub-ontologies rel instance 'relation)
      instance))

(defun check-proofs-by-consistency (name options)
  (loop with bad-proofs-by-statements
        for value in (init-arg-value :no-proofs-by  options)
        do
        (unless (init-arg-value value options)
          (pushnew value bad-proofs-by-statements))
        finally
        (when bad-proofs-by-statements
          (if (cdr bad-proofs-by-statements)
            (warn 
             "The following keywords, <~{~s, ~}~s>, have been declared non-operational through a :no-proofs-by statement, but they do not appear in the specification of ~s"
             (butlast bad-proofs-by-statements)
             (car (last bad-proofs-by-statements))
             name)
            (warn "The ~s keyword has been declared non-operational through a :no-proofs-by statement, but does not seem to appear in the specification of ~s"
                  (car bad-proofs-by-statements)
                  name)))))
  
(defun add-own-slot-of (rel class)
  (pushnew class (own-slot-of (get-relation rel))))

(defun remove-own-slot-of (rel class)
  (let ((structure (get-relation rel)))
    (setf (own-slot-of structure)
          (remove class (own-slot-of structure)))))



;;;MAYBE-PROCESS-SUFFICIENT-&-IFF-DEF-ENTRIES --- modified by Mauro
(defmethod  maybe-process-sufficient-&-iff-def-entries ((obj ocml-relation))
  (with-slots (sufficient iff-def name schema prove-by exclusive-prove-by
                          no-proofs-by)
      obj
    (when sufficient
      (unless (member :sufficient no-proofs-by)
        ;; (unless (find-bc-rule name)
        ;;  (add-backward-rule name "" () (length schema)))
        (setf sufficient
	      (make-bc-rule-clause (list (cons name schema)
				         'if
				         sufficient)))))
    
    (when prove-by
      ;; (unless (find-bc-rule name)
      ;;   (add-backward-rule name "" () (length schema)))
      (setf prove-by
	    (make-bc-rule-clause (list (cons name schema)
				       'if
				       prove-by))))
    (when iff-def
      (unless (member :iff-def no-proofs-by)
        ;; (unless (find-bc-rule name)
        ;;   (add-backward-rule name "" () (length schema)))
        (setf iff-def
              (make-bc-rule-clause (list (cons name schema)
                                         'if
				         iff-def)
                                   'iff-def-clause))))
    (when exclusive-prove-by
        (setf iff-def
              (make-bc-rule-clause (list (cons name schema)
                                         'if
				         exclusive-prove-by)
                                   'iff-def-clause)))
    
    ))

;(defun set-own-slots (rel-instance old new)
*;  (with-slots (name own-slots) rel-instance
;    (loop for exp in old
;          do
;          (unassert1 `(,(car exp) ,name ,(second exp))))
;    (loop for exp in new
;          do
;          (tell1 `(,(car exp) ,name ,(second exp))))
;    (setf own-slots new)))

(defun find-or-create-relation (predicate arity &optional relation-spec)
  (or (get-relation predicate)
      (prog1
        (apply #'make-ocml-relation  predicate :arity arity
                          :schema (loop for i from 1 to arity
                                        collect (make-new-var))
                          relation-spec)
        (ocml-record-source-file predicate 'ocml-relation))))

;;;INITIALIZE-INSTANCE :AFTER OCML-RELATION
(defmethod initialize-instance :after ((relation ocml-relation) &rest initargs)
  (declare (ignore initargs))
  (with-slots (name schema arity ) relation
    (enforce-arity-schema-consistency relation name schema arity)
    (add-to-relations-directory name relation)))
    ;;;;(maybe-update-definition-in-super-ontologies relation)))


;;;CLEAR-ALL-DEFINED-BY-RULE-ENTRIES
(defun clear-all-defined-by-rule-entries ()
  (maphash #'(lambda (key relation)
               (declare (ignore key))
               (reset-defined-by-rule-entry relation))
           *defined-relations*))


(defmethod set-defined-by-rule-entry ((relation ocml-relation) rule)
  (with-slots (defined-by-rule) relation
    (setf defined-by-rule rule)))

(defmethod add-defined-by-rule-entry ((relation ocml-relation) rule)
  (with-slots (defined-by-rule) relation
    (setf defined-by-rule (cons rule defined-by-rule ))))

(defmethod remove-defined-by-rule-entry ((relation ocml-relation) rule)
  (with-slots (defined-by-rule) relation
    (setf defined-by-rule (remove rule defined-by-rule ))))

(defmethod reset-defined-by-rule-entry ((relation ocml-relation))
  (with-slots (defined-by-rule) relation
    (setf defined-by-rule nil)))

;;;ADD-SLOT-OF-ENTRY
(defmethod add-slot-of-entry ((relation ocml-relation) class)
  (with-slots (slot-of) relation
    (unless (member class slot-of)
      (setf slot-of (cons class slot-of)))))
    ;;;;(pushnew class slot-of)))

(defmethod add-local-slot-of-entry ((relation ocml-relation) class)
  (with-slots (local-slot-of) relation
    (unless (member class local-slot-of)
      (setf local-slot-of (cons class local-slot-of)))))

;;;REMOVE-SLOT-OF-ENTRY
(defmethod remove-slot-of-entry ((relation ocml-relation) class)
  (with-slots (slot-of) relation
    (setf slot-of
          (remove class slot-of))))

(defmethod remove-local-slot-of-entry ((relation ocml-relation) class)
  (with-slots (local-slot-of) relation
    (setf local-slot-of
          (remove class local-slot-of))))

;;;SLOTP ---True if a relation is a slot of some domain class
(defmethod slotp ((relation ocml-relation))
  (with-slots (slot-of) relation
    slot-of))

;;;ENFORCE-ARITY-SCHEMA-CONSISTENCY
;;;This is used to make sure that arity and schema slots are consistent in
;;;functions and relations
(defun enforce-arity-schema-consistency (obj name schema arity &aux l)
  ;;;;(when schema
  (setf l (length schema))
  (if (and arity
           (not (= arity l)))
    (progn 
      (ocml-warn
       "Current arity of ~S ~S is ~S. Setting it to ~S to be consistent with schema ~S"
       (type-of obj) name arity (length schema) schema)
      (setf (arity obj) l))
    (unless arity
      (setf (arity obj) l))))


(defun rename-schema (relation)
  (mapcar #'(lambda (var)
                (make-new-var var))
	  (schema (get-relation relation))))

;;;GET-RELATION-TYPE ---This returns the 'type' of a relation.  This can be
;;;  :class - if the relation is a domain class
;;;  :slot  - if the relation is a slot
;;;  :lisp  - if it is defined by means of a lisp attachment
;;;  :predicate - all other cases
(defmethod get-relation-type ((relation ocml-relation))
  (with-slots (slot-of lisp-fun name) relation
    (cond (lisp-fun
           :lisp)
          (slot-of
           :slot)
          ((get-domain-class name)
           :class)
          (t
           :predicate))))

;;;CACHE-VALUE-INTERNAL --Adds a new cached value to the cache table of a relation
(defun cache-value-internal (rel key value)
  (setf (relation-cache-table rel)
        (cons (cons key value)
              (relation-cache-table rel))))

;;;FETCH-CACHED-VALUE --Retrieves a previously cached value from the cache table of a relation
(defun fetch-cached-value (rel key index-table)
  (Let ((value (right-value key (relation-cache-table rel):test #'equal)))
    (when value
      (loop for pair in index-table
            do
            (setf value (subst (car pair)(cdr pair)value)))
      value)))


;;;ADD-ALPHA-NODE-TO-RELATION ---Links an alpha node to a relation.  If the relation is the name
;;;of a class the alpha node is also added to the subclasses of the relation
(defmethod add-alpha-node-to-relation ((relation ocml-relation) node &optional type)
  (with-slots (fc-nodes name) relation
    (setf fc-nodes (nconc fc-nodes (list node)))
    (when (eq type :class)
      (dolist (subclass (current-subclasses  (get-ocml-class name)))
        (add-alpha-node-to-relation (get-relation (name subclass)) node)))))

(defmethod add-indirect-fc-node ((relation ocml-relation) node)
  (with-slots (indirect-fc-nodes) relation
    (push node indirect-fc-nodes)))


;;;CLEAR-ALL-ALPHA-NODES
(defun clear-all-alpha-nodes ()
  (maphash #'(lambda (key relation)
               (declare (ignore key))
               (reset-alpha-nodes relation t))
           *defined-relations*))

;;;RESET-ALPHA-NODES  
(defmethod reset-alpha-nodes ((relation ocml-relation) &optional recur? &aux class)
  (with-slots (fc-nodes indirect-fc-nodes name) relation
    (setf indirect-fc-nodes nil)
    (setf fc-nodes nil)
    (when (and recur? (setf class (get-domain-class name)))
      (dolist (subclass (current-subclasses class))
        (reset-alpha-nodes (get-relation subclass))))))

;;;REMOVE-ALPHA-NODE RELATION
(defmethod remove-alpha-node ((relation ocml-relation)alpha-node &optional recur? 
                              &aux class)
  (with-slots (fc-nodes name) relation
    (setf fc-nodes (remove alpha-node fc-nodes))
    (when (and recur? (setf class (get-domain-class name)))
      (dolist (subclass (current-subclasses  class))
        (remove-alpha-node (get-relation subclass)alpha-node)))))

(defmethod remove-alpha-node ((relation (eql nil)) alpha-node &optional recur?)
  ;; Allegro (reasonably) believes that RELATION *is* used, in the
  ;; specialiser.
  (declare (ignore #-:allegro relation alpha-node recur?)))

(defmethod remove-indirect-alpha-node ((relation ocml-relation)alpha-node)
  (with-slots (indirect-fc-nodes) relation
    (setf indirect-fc-nodes (remove alpha-node indirect-fc-nodes))))


;;;FIND-RELATION-INSTANCE
(defmethod find-relation-instance ((relation ocml-relation) args)
  (with-slots (relation-instances)relation
    (find args relation-instances :test #'(lambda (x y)
                                                   (equal x (args y))))))

;;;ADD-RELATION-INSTANCE
(defmethod add-relation-instance ((relation ocml-relation) instance)
                                 ;;;;; (instance relation-instance))
  (with-slots (relation-instances name)relation
   ;;;;; (with-slots (predicate args) instance
    (push instance relation-instances)
     (when *check-constraints*
       (check-relation-instance-constraints instance relation))
    (tell-fc-rules name (args instance))))

;;;REMOVE-RELATION-INSTANCE-GEN
(defmethod remove-relation-instance-gen ((relation ocml-relation) args
                                         &optional contains-vars?)
  (if contains-vars?
      (with-slots (relation-instances name)relation
        (loop with deleted-relins
              for rel-instance in relation-instances
              unless (eq :fail
                         (match (args rel-instance) args))
              do
              (push rel-instance deleted-relins)
              finally
              (setf relation-instances (Set-difference relation-instances
                                                       deleted-relins))
              (dolist (rel-instance deleted-relins)
                (unassert-from-fc-rules  name (args rel-instance)))))
      (remove-relation-instance relation args)))

;;;REMOVE-RELATION-INSTANCE
(defmethod remove-relation-instance ((relation ocml-relation) args)
  (with-slots (relation-instances name)relation
    (setf relation-instances
          (remove args relation-instances
                  :test #'(lambda (x y)
			    (equal x (args y)))
                  :count 1))
    (unassert-from-fc-rules  name args)))

;(defmethod remove-relation-instance ((relation ocml-relation) args
;                                     &optional contains-vars?)
;  (with-slots (relation-instances name)relation
;    (setf relation-instances
;          (remove args relation-instances
;                  :test (if contains-vars?
;			    #'(lambda (x y)
;				(not (eq :fail
;					 (match (args y) x))))
;			    #'(lambda (x y)
;				(equal x (args y))))))
;    (unassert-from-fc-rules  name args)))
    

;;;CREATE&ADD-RELATION-INSTANCE
(defun create&add-relation-instance (pred-structure exp original-form documentation)
  (add-relation-instance
   pred-structure
   (make-relation-instance exp original-form documentation)))
  
  
;;;MAYBE-ADD-RELATION-INSTANCE
(defun maybe-add-relation-instance (pred-structure args original-form documentation
                                              &aux (exp (cons (car original-form) args)))
  ;;;A structure for the relation must already exist
  (cond ((find-relation-instance pred-structure args)
         (when (tracing-this-assertion? exp)
	 (format t "~%~S has already been asserted" exp)))
          (t
           (create&add-relation-instance pred-structure exp original-form documentation))))

;;;redefined to take into account multiple instances with the same name - 23-6-99
;;;redefined 20-7-99
(defmethod maybe-add-slot-assertion ((relation ocml-relation) args original-form)
  ;;relation is a slot
  (with-slots (name slot-of) relation
    ;;slot-of = local slot of
    (destructuring-bind (instancen value) args
      (let (instance 
            (instances (find-all-current-instances-named-x-of-these-classes
                        instancen
                        (mapcar #'name (filter-active-classes  slot-of)))))

           ;;; (find-current-direct-instances-in-classes 
           ;;;             instancen 
           ;;;             (filter-active-classes  slot-of))))
        (if (cdr instances)
          (error "not clear which of the following instances should have slot ~s set to ~S: ~{~a  ~}"
                 name value (mapcar #'(lambda (inst)
                                        (format nil "~%instance ~s of class ~s;"
                                                (name inst)(name(parent-class inst))))
                                     instances))
          (progn
            (setf instance (car instances))
            (if instance
              (maybe-add-slot-value-to-instance instance name value)
              (error
               "trying to add a slot value to undefined instance ~S..when parsing assertion ~S"
               instancen original-form))))))))



;;;MAYBE-DELETE-SLOT-ASSERTION ---
;;(defmethod  maybe-delete-slot-assertion ((relation ocml-relation) args original-form) ;;;no-checks)
;;  (declare (ignore original-form))
;;  (with-slots (name slot-of ) relation
;;    (destructuring-bind (instancen value) args
;;      (cond ((variable? instancen)
;;             (if (variable? value)
;;                 (loop for class in (filter-active-classes slot-of)
;;                       do
;;		       (remove-all-slot-values-from-direct-instances
;;                        class
;;                        name
;;                        ;;;;no-checks
;;                        ))
;;                 (loop for class in (filter-active-classes slot-of)
;;                       do
;;		       (remove-slot-value-from-direct-instances
;;                        class
;;                        name
;;                        value
;;                       ;;;;no-checks
;;                        ))))
;;	    ((variable? value)
;;             (loop for instance in (find-current-direct-instances-in-classes 
;;                                    instancen (filter-active-classes slot-of))
;;                   do
;;                   (remove-local-slot-values instance name))) ;;;; no-checks))))
;;	    (t
;;             (loop for instance in (find-current-direct-instances-in-classes 
;;                                    instancen (filter-active-classes slot-of))
;;                   do
;;                   (maybe-remove-slot-value instance name value)))))))


(defmethod  maybe-delete-slot-assertion ((relation ocml-relation) args original-form) ;;;no-checks)
  (declare (ignore original-form))
  (with-slots (name local-slot-of) relation
    (destructuring-bind (instancen value) args
      (cond ((variable? instancen)
             (if (variable? value)
               (remove-all-slot-values-from-all-instances-of-these-classes
                (filter-active-classes local-slot-of) name)
               (remove-slot-value-from-all-instances-of-these-classes
                (filter-active-classes local-slot-of) name value)))
            ((variable? value)
             (remove-all-slot-values-from-all-instances-named-x-of-these-classes 
              instancen
              (filter-active-classes local-slot-of)
              name))
            (t
             (remove-slot-value-from-all-instances-named-x-of-these-classes 
              instancen
              (filter-active-classes local-slot-of)
              name value))))))



 
;;;GENERATE-CANDIDATES OCML-RELATION
;;;returns a list (lisp-fun instances classes clauses)
;(defmethod generate-candidates ((relation ocml-relation) pred args &optional ignore-instances 
;                                &aux class classes instances)
;  (with-slots (slot-of defined-by-rule relation-instances lisp-fun 
;                       sufficient iff-def prove-by) 
;              relation
;    (cond (lisp-fun)
;          (prove-by
;           (values nil nil nil (list prove-by)))
;          (iff-def
;           (values nil nil nil (list iff-def)))
;          (t                    ;;Is it defined by a lisp fun?
;           (Progn
;             (cond ((setf instances (filter relation-instances
;                                            #'(lambda (inst)
;                                                (member  (home-ontology inst)
;                                                         *current-ontologies*)))))
;		   (slot-of
;                    (when (= (length args)    ;Slots are binary relations.  There is no point in 
;                             2)               ;generating candidates if the goal hasn't got 2 args.
;                      (let ((instance-id (car args)))
;                        (cond ((and (atom instance-id)
;                                    (not (variable? instance-id)))
;                               ;;If the first arg to the slot is not a variable, then there is only one
;                               ;;plausible candidate: an instance named <instance-id>
;                               (setf instances            
;                                     (find-current-direct-instances-in-classes 
;                                      instance-id 
;                                      (filter-active-classes slot-of)))
;                               ;; (when instances
;                               ;;   (setf instances (List instances)))
;                               )
;                              (t
;			       (setf classes (filter-active-classes slot-of)))))))
;                   
;                   ((setf class (get-domain-class pred))
;                    (unless ignore-instances 
;                      ;;if teh goal is something like (class ?X), then we retrieved all the instances
;                      ;;before going down the subclass-of hierarchy.  Therefore we do need to get them again
;                      (setf instances (get-current-instances class)
;                                      ;;;;;;(get-current-direct-instances class)
;                            ))
;                          ;;;;(append instances (get-direct-instances class))
;                    ;;;;(setf classes (current-direct-subclasses class))
;                    ))
;             ;;;;;;(cons class
;             ;;;;;;  (subclasses nil class))))
;             
;             (let* ((current-rules (filter defined-by-rule
;                                           #'(lambda (rule)
;                                               (member  (home-ontology rule)
;                                                        *current-ontologies*))))
;                    (clauses (when current-rules
;                               (if (cdr current-rules)
;                                 (apply #'append
;                                        (mapcar  #'clauses current-rules))
;                                 (clauses (car current-rules))))))
;               (when sufficient
;                 (setf clauses (cons sufficient clauses)))
;               ;;(when iff-def 
;               ;;;(setf clauses (cons iff-def clauses)))              
;               (values nil instances classes clauses)))))))


;;;GENERATE-CANDIDATES
;;;returns a list of 4 elements (lisp-fun instances classes clauses)
(defmethod generate-candidates ((relation ocml-relation) pred args 
                                env subclass-state 
                                &aux class  instances classes)
  (with-slots (slot-of local-slot-of defined-by-rule relation-instances lisp-fun 
                       sufficient iff-def prove-by) 
              relation
    (cond (lisp-fun)
          ;;(prove-by
          ;; (values nil nil nil (list prove-by)))
         ;; (iff-def
          ;; (values nil nil nil (list iff-def)))
          ((and slot-of
                (not (= (length args)  ;Slots are binary relations.  
                        2)))          ;not much point in proceeding
           nil)
          (t
           (cond
            (slot-of 
             (let ((instance-id (if (atom (car args))
                                  (lookup-or-self 
                                  (car args) env)
                                  (car args))))
                                  
               (cond ((and (atom instance-id)
                           (not (variable?  ;;let's try not to get lots of useless instances.....
                                 instance-id )))
                      
                      ;;If the first arg to the slot is not a variable, then there is only one
                      ;;plausible candidate: an instance named <instance-id>
                      (setf instances            
                            (find-all-current-instances-named-x-of-these-classes
                             instance-id 
                             (mapcar #'name (filter-active-classes local-slot-of)))))
                     (t
                      (setf instances            
                            (find-all-current-instances-in-classes 
                             (filter-active-classes local-slot-of)))))))
            ((setf class (get-domain-class pred))
             (unless subclass-state
               (setf classes (remove-duplicates (current-subclasses class)))
               (setf instances (get-current-instances class))))
            (t
             (setf instances (filter relation-instances
                                     #'(lambda (inst)
                                         (member  (home-ontology inst)
                                                  *current-ontologies*))))))
           (let* ((current-rules (filter defined-by-rule
                                         #'(lambda (rule)
                                             (member  (home-ontology rule)
                                                      *current-ontologies*))))
                  (clauses (when current-rules
                             (if (cdr current-rules)
                               (apply #'append
                                      (mapcar  #'clauses current-rules))
                               (clauses (car current-rules))))))
               (when (bc-clause? sufficient)
                 (setf clauses (cons sufficient clauses)))
               
               (when (bc-clause? prove-by)
                 (setf clauses (cons prove-by clauses)))
               
               ;;modified yet again - Enrico 22/8/02
               (when (bc-clause? iff-def)
                 (setf clauses (List iff-def)
                       classes nil))
                ;;; (setf clauses (cons iff-def clauses)))
               
               
               (values nil instances classes clauses))))))




;;;FIND-ALPHA-NODE-CANDIDATES
(defmethod find-alpha-node-candidates ((relation ocml-relation)   &aux class)
  (with-slots (slot-of  relation-instances lisp-fun name) relation
    (unless lisp-fun                     ;;If it is defined by a lisp fun, we ignore it
      (if relation-instances
          (values relation-instances :relation-instances)
          (if slot-of
              (values 
               (loop for class in (filter-active-classes slot-of)
		     appending (get-current-direct-instances class))
               :slot)
              (when (setf class (get-domain-class name))
                (values (get-current-instances class)
                        :class)))))))


;;;*******************************************************************

;;;RELATION-INSTANCE 
(defclass relation-instance (basic-ocml-object)
  ((predicate :initarg :predicate)
   (args :initarg  :args :accessor args)
   (original-form :initarg :original-form)
   )
  (:documentation "This class represents the existing relation instances (i.e. assertions)"))

(defun make-relation-instance (parsed-form original-form documentation)
  (make-instance 'relation-instance
                 :predicate (car parsed-form)
		 :args (cdr parsed-form)
                 :original-form original-form
		 :documentation documentation))

(defun relation-instance? (thing)
  (typep thing 'relation-instance))


(defun def-relation-instances-internal (documentation exps)
  (unless (stringp documentation)
    (setf exps (cons documentation exps)
          documentation ""))
  (dolist (exp exps)
    (ocml-record-source-file exp 'ocml-relation-instances)
    (tell1 exp documentation)))

;;relation-instance 'name' is in fact the expression
(defun get-relation-instance (name)
  (let* ((relation-name (car name))
         (relation (get-ocml-relation relation-name)))
    (when relation
      (find name (get-direct-relation-instances relation)
            :key #'get-relation-instance-expression
            :test #'equal))))

(defun get-ocml-relation (name)
  (gethash name *defined-relations*))

(defun get-direct-relation-instances (structure)
  (relation-instances structure))

;;; XXX This is currently only useful for helping
;;; #'copy-hash-table-entry merge the rules from multiple relations.
(defun copy-relation (original)
  "Produce a new relation mirroring but not sharing state with ORIGINAL."
  (let ((copy (make-instance 'ocml-relation)))
    (setf (arity copy) (arity original)
	  (schema copy) (schema original)
	  (avoid-infinite-loop copy) (avoid-infinite-loop original)
	  (home-ontology copy) *current-ontology*
	  (name copy) (name original))
    copy))
