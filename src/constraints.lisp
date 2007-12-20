(in-package :ocml)

(define-condition <constraint-violation> (simple-warning)
  ())

(defun warn-violation (format-control &rest format-arguments)
  (warn (make-condition '<constraint-violation>
			:format-control format-control
			:format-arguments format-arguments)))

;;;;to check change-instance-parent-class

(defun check-relation-instance-constraints (instance relation &optional (force-checking? nil))
  (loop with violations = nil
     for constr in (fetch-constraints-from-relation relation)
     for flag = (check-constraint constr (args instance))
     do (unless flag
	  (push constr violations))
     finally
     (when violations
       (if (and (boundp '*pending-constraints*) *asserting-own-slots* (not force-checking?))
	   (pushnew (list :relation-instance-constraints instance relation)
		    *pending-constraints* :test #'equal)
	   (warn-violation "Instance ~S violates constraints: ~%~{ ~s~%~}"
			   (cons (name relation) (args instance)) violations)))))

(defun check-slot-assertion-constraints (instance slot values
					 &optional (check-slot-type? t)
					 (force-checking? nil))
  (when (and check-slot-type?
             (not (slot-renamed? slot (renaming-chains (parent-class instance)))))
    (check-slot-type instance slot values))
  (loop with name = (name instance)
     with violations = nil
     for constr in (fetch-constraints-from-relation (get-relation slot))
     do (loop for value in values
	   for flag = (check-constraint constr (list name value)
					(and (boundp '*pending-constraints*)
					     (not force-checking?)))
	   do
	   (unless flag
	     (push (List value (transform-slot-constraint slot constr)) violations)))
     finally
     (when violations
       (if (and (boundp '*pending-constraints*)
		(not force-checking?))
	   (pushnew (list :instance-slot-constraints instance slot) *pending-constraints* :test #'equal)
	   (warn-violation "Instance ~S violates constraints: ~%~{ ~s~%~}" name violations)))))

;;;CHECK-INSTANCE-CONSTRAINTS - top level function to check the constraints applicable to a
;;;newly (re-)defined instance
(defun check-instance-constraints (instance parent) ;;;  &optional (force-checking? nil))
  (loop with violations = nil
     for constr in (fetch-applicable-constraints parent)
     for flag = (check-constraint constr (list (name instance)))
     do
     (unless flag
       (push (list constr (find-constraint-in-ancestor constr instance))
	     violations))
     finally
     (when violations
       (warn-violation "Instance ~S violates constraints: ~%~{ ~s~%~}" (name instance) violations))))

;      (if (and (boundp '*pending-constraints*)
;               (not force-checking?))
;        (pushnew (list :instance-constraints instance) *pending-constraints* :test #'equal)
;        ))))

;;;CHECK-INSTANCE-SLOT-CONSTRAINTS
;;;This function is called after an instance is created or an ancestor class is redefined
;;;It checks that all slot values in all domain slots satisfy the applicable constraints
;;;
(defun check-instance-slot-constraints (instance)
  (loop with chains = (renaming-chains (parent-class instance))
        for slot in (domain-slots instance)
        do
        (unless (slot-renamed? slot chains)
          (check-slot-assertion-constraints instance slot (get-slot-values instance slot)))))


(defun transform-slot-constraint (slot constr)
  (destructuring-bind (kappa schema body) constr
    (list kappa schema
          (list '=> (cons slot schema) body))))

(defun check-constraint (constraint args &optional suppress-warning?)
  (let ((*ignore-undefined-relations* (if suppress-warning?
					  :ignore
					  :warn)))
    (apply #'holds? (cons constraint args))))

(defun fetch-applicable-constraints (class)
  (let ((supers (domain-superclasses class)))
    (mapcan* #'(lambda (c)
                (fetch-constraints-from-class c))
            (cons class supers))))

(defun fetch-constraints-from-class (class)
  (let ((ontology))
    (if (eq *current-ontology* (home-ontology class))
      (fetch-constraints-from-relation (get-relation (name class)))
      (unwind-protect
        (progn
          (setf ontology *current-ontology*)
          (switch-to-ontology (home-ontology class))
          (fetch-constraints-from-relation (get-relation (name class)))
        )
        (unless (eq *current-ontology* ontology)
          (switch-to-ontology ontology))))))


(defun fetch-constraints-from-relation (rel)
  ;;(print *current-ontology*)
  "Fetch all applicable constraints associated with a relation.
   Note that :no-op and :axiom-def constraints are not returned.  The
   former because they are not operational (and therefore we do not
   check them.  The latter because they are not meant to be checked
   every time an assertion is made, but are properties of the
   relation, to be checked under user/developer control (e.g. an axiom
   def may specify that a relation defines a partial order"
  (Let ((constraint (constraint rel))
        (iff-def (iff-def rel))
        )
    (if (and constraint
               (not (member :constraint
                            (no-proofs-by rel))))
      (setf constraint `(kappa ,(schema rel) ,constraint))
      (setf constraint nil))
    (if (and iff-def
               (not (member :iff-def (no-proofs-by rel))))
      (setf iff-def `(kappa ,(schema rel) ,(first (bc-clause-antecedents iff-def))))
      (setf iff-def nil))
    (remove-if #'null (list constraint iff-def))))

(defun find-constraint-in-ancestor (constraint instance)
  (let* ((parent (parent-class instance))
         (supers (domain-superclasses parent)))
    (loop for c in (cons parent supers)
          for constraints = (fetch-constraints-from-class c)
          do
          (when (member constraint constraints :test #'equal)
            (return (name c))))))


;;; XXX Can be replaced with with-ontology in the correct places.
(defun validate-classes-in-ontology-internal (ontology)
  "Returns all the undefined classes used as type constraints."
  (let ((current-ontology *current-ontology*))
    (cond ((eq current-ontology ontology)
           (validate-classes-in-current-ontology))
          (t
           (unwind-protect
             (progn
               (switch-to-ontology ontology)
               (validate-classes-in-current-ontology))
             (switch-to-ontology current-ontology))))))

;;;VALIDATE-CLASSES-IN-CURRENT-ONTOLOGY
(defun validate-classes-in-current-ontology ()
  (loop with bad-classes
        with all-classes = (all-ocml-classes t)
        for class in all-classes
        do
        (loop for slot in (domain-slots class)
              for types = (get-slot-type class slot)
              do
              (loop  for type in types
                     do
                     (unless (member (get-ocml-class type)  all-classes)
                       (pushnew type bad-classes))))
        finally
        (return bad-classes)))

;;;FIND-UNTYPED-SLOTS-IN-CURRENT-ONTOLOGY
(defun find-untyped-slots-in-current-ontology ()
  (loop with bad-slots
        with all-classes = (all-ocml-classes t)
        for class in all-classes
        do
        (loop for slot in (local-slots  class)
              for types = (get-slot-type class slot)
              do
              (unless types
                (push (cons class slot) bad-slots)))
        finally
        (return bad-slots)))
