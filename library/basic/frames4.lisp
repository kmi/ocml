;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

;;;CLASS
(def-class CLASS (relation) 
  "The class of OCML classes"
  :lisp-fun  #'(lambda (x env)
               (let ((y (unbound-variable? x env)))
                   (if y
                     (mapcar #'(lambda (rel)
                                 (cons (cons y rel) env))
                             (all-ocml-classes))
                     (if (get-ocml-class 
                          (instantiate x env))
                       (list env)
                       :fail)))))
               
(def-class INSTANCE (term) 
  "This is true for instances defined using def-instance"
  :lisp-fun #'(lambda (x env)
                (if (find-current-instance (instantiate x env))
                  (list env)
                  :fail)))


;;;RELATION  INSTANCE-OF
(def-relation instance-of (?x ?C)
  "This definition generalises the notion of instance.
   ?I is an instance of a class ?c, if (holds ?c ?i)
   is true"
   :constraint (class ?c)
   :iff-def (and (class ?c)
                 (holds ?c ?x))
   :prove-by (or (and (variable-bound ?c)
                      (class ?c)
                      (holds ?c ?x))
                 (and (not (variable-bound ?c))
                      (variable-bound ?x)
                      (not (= (the-parent ?x)
                              :nothing))
                      (member ?c
                              (cons (the-parent ?x)
                                    (all-superclasses (the-parent ?x)))))
                 (and (not (variable-bound ?c))
                      (not (variable-bound ?x))
                      (exec (output "Trying to prove instance-of with 2 unbound variables....you will definitively lose..."))
                      (class ?c)
                      (instance-of ?x ?c)))
   :no-proofs-by (:iff-def))



;;;DIRECT-INSTANCE-OF
(def-relation direct-instance-of (?x ?C)
   :constraint (class ?c)
   :iff-def (and  (instance-of ?x ?c)
                  (not (exists ?c2
                               (and (subclass-of ?c2 ?c)
                                    (instance-of ?x ?c2)))))
   :prove-by (or (and (variable-bound ?c)
                      (class ?c)
                      (member ?x (all-direct-instances ?c)))
                 (and (not (variable-bound ?c))
                      (variable-bound ?x)
                      (= (the-parent ?x) ?c)
                      (not (= ?c :nothing)))
                 (and (not (variable-bound ?c))
                      (not (variable-bound ?x))
                      (exec (output "Trying to prove direct-instance-of with 2 unbound variables....you will definitively lose..."))
                      (class ?c)
                      (direct-instance-of ?x ?c)))
   :no-proofs-by (:iff-def))

  
    

;;;ALL-INSTANCES
(def-function all-instances (?c) -> ?instances
   :constraint (class ?c)
   :lisp-fun #'(lambda (c)
                 (mapcar #'name (all-current-instances c))))


;;;ALL-DIRECT-INSTANCES
(def-function all-direct-instances (?c) -> ?instances
   :constraint (class ?c)
   :lisp-fun #'(lambda (c)
                 (mapcar #'name (all-current-direct-instances c))))


;;;NEW-INSTANCE
(def-function new-instance (?type ?role-value-pairs)
    :lisp-fun #'(Lambda (type pairs) 
                 (name 
                  (define-domain-instance (GENTEMP "INSTANCE") type ""
                             pairs))))



(def-function THE-PARENT (?i) -> ?c
  :constraint (and (class ?c)(instance ?i) (direct-instance-of ?i ?c))
  :lisp-fun #'(lambda (i)
                (if (find-current-instance i)
                  (name (parent-class (find-current-instance i)))
                  :nothing)))

;;;THE-PARENT-NEW - this is the right one to use, the one above
;;;should be discontinued
(def-function THE-PARENT-NEW (?i ?type) -> ?c
  :constraint (and (class ?c)(instance ?i) (direct-instance-of ?i ?c))
  :lisp-fun #'(lambda (i type)
                (if (find-current-instance i type)
                  (name (parent-class (find-current-instance i type)))
                  :nothing)))



(def-function all-subclasses-in-all-ontologies (?class)
  :constraint (class ?class)
  :lisp-fun #'(lambda (class)
                  (let ((class-s (get-ocml-class class)))
                   (if class-s
                     (mapcar #'name (subclasses class-s))))))

(def-function all-subclass-structures-in-all-ontologies (?class)
  :constraint (class ?class)
  :lisp-fun #'(lambda (class)
                  (let ((class-s (get-ocml-class class)))
                   (if class-s
                    (subclasses class-s)))))



(def-function ALL-SUBCLASSES (?class) -> ?subs
  "True if ?subs is all the subclasses of ?class.  
   ?class  is the name of a class.
   ?subs is a list of class names."
   :constraint (class ?class)
   :lisp-fun  #'(lambda (class)
                  (let ((class-s (get-ocml-class class)))
                   (if class-s
                     (mapcar #'name (current-subclasses class-s))))))


(def-function ALL-DIRECT-SUBCLASSES (?class) -> ?subs
   :constraint (class ?class)
   :lisp-fun  #'(lambda (class)
                  (let ((class-s (get-ocml-class class)))
                   (if class-s
                     (mapcar #'name (current-direct-subclasses class-s))))))

(def-function ALL-SUPERCLASSES (?class) -> ?supers
  :constraint (class ?class)
  :lisp-fun  #'(lambda (class)
                 (let ((class-s (get-ocml-class class)))
                   (if class-s
                     (mapcar #'name (domain-superclasses class-s))))))

(def-function ALL-DIRECT-SUPERCLASSES (?class) -> ?subs
   :constraint (class ?class)
   :lisp-fun  #'(lambda (class)
                  (let ((class-s (get-ocml-class class)))
                   (if class-s
                     (mapcar #'name (direct-domain-superclasses class-s))))))



;;;SUBCLASS-OF
(def-relation subclass-of (?sub ?c)
   :constraint (and (class ?sub)(class ?c))
   :iff-def (superclass-of ?c ?sub))

;;;DIRECT-SUBCLASS-OF
(def-relation direct-subclass-of (?sub ?c)
   :constraint (and (class ?sub)(class ?c))
   :iff-def (direct-superclass-of ?c ?sub))


(def-relation SUPERCLASS-OF (?super ?c)
   "The inverse of subclass-of"
   :constraint (and (class ?super)(class ?c))
   :iff-def (member ?super (all-superclasses ?c))
   :prove-by (or (and (variable-bound ?c)
                      (member ?super (all-superclasses ?c)))
                 (and (variable-bound ?super)
                      (NOT (variable-bound ?c))
                      (member ?c (all-subclasses ?super)))
                 (and 
                      (not (variable-bound ?super))
                      (not (variable-bound ?c))
                      (class ?super)(class ?c)
                      (superclass-of ?super ?c)))
   :no-proofs-by (:iff-def))
                 
                      

(def-relation DIRECT-SUPERCLASS-OF (?super ?c)
   "The inverse of subclass-of"
   :constraint (and (class ?super)(class ?c))
   :iff-def (direct-subclass-of ?c ?super)
   :prove-by (or (and (variable-bound ?c)
                      (member ?super (all-direct-superclasses ?c)))
                 (and (variable-bound ?super)
                      (NOT (variable-bound ?c))
                      (member ?c (all-direct-subclasses ?super)))
                 (and 
                      (not (variable-bound ?super))
                      (not (variable-bound ?c))
                      (class ?super)(class ?c)
                      (direct-superclass-of ?super ?c)))
   :no-proofs-by (:iff-def))

(def-class CLASS-PARTITION () ?set-of-classes
  "A set of mutually disjoint classes.  Disjointness of
   classes is a special case of disjointness of sets."
  ()
  :iff-def (and (variable-bound ?set-of-classes)
                (set ?set-of-classes)
                (forall ?C
                        (=> (member ?C ?set-of-classes)
                            (class ?C)))
                (forall (?C1 ?C2)
                        (=> (and (member ?C1 ?set-of-classes)
                                 (member ?C2 ?set-of-classes)
                                 (not (= ?C1 ?C2)))
                            (forall (?i)
                                    (=> (instance-of ?i ?C1)
                                        (not (instance-of ?i ?C2))))))))


(def-relation SUBCLASS-PARTITION (?C ?class-partition)
   "A subclass-partition of a class C is a set of
    subclasses of C that are mutually disjoint."

   ;;;;()
   :iff-def (and (class ?C)
                 (class-partition ?class-partition)
                 (forall ?subclass
                         (=> (element-of ?subclass ?class-partition)
                             (subclass-of ?subclass ?C)))))

(def-relation EXHAUSTIVE-SUBCLASS-PARTITION (?C ?class-partition)
   "A subrelation-partition of a class C is a set of
    mutually-disjoint classes (a subclass partition) which covers C.
    Every instance of C is is an instance of exactly one of the subclasses
    in the partition."


   :iff-def (and (subclass-partition ?C ?class-partition)
                 (forall ?instance
                         (=> (instance-of ?instance ?C)
                             (exists ?subclass
                                     (and (element-of ?subclass ?class-partition)
                                          (instance-of ?instance ?subclass)))))))


;;;SLOT
(def-class slot (binary-relation) ?r
   "The class of OCML slots"
   :constraint (binary-relation ?r)
   :lisp-fun #'(lambda (x env)
                 (let ((y (unbound-variable? x env)))
                   (if y
                     (mapcar #'(lambda (rel)
                                   (cons (cons y (name rel)) env))
                             (filter (all-relations t)
                                     #'(lambda (rel)
                                         (slot-of rel))))
                     (let ((rel (get-relation (instantiate x env))))
                       (if rel
                         (if (slot-of rel)
                           (list env)
                           :fail)
                         :fail))))))

;   :lisp-fun #'(lambda (x env)
;                 (let ((rel (get-relation (instantiate x env))))
;                   (if rel
;                       (if (slot-of rel)
;                           (list env)
;                           :fail)
;                       :fail))))


(def-relation HAS-SLOT-VALUE (?instance ?s ?value)
   :iff-def (and (slot ?s)
                 (holds  ?s ?instance ?value)))

(def-function THE-SLOT-VALUE (?instance ?s )
   :body (the ?v (has-slot-value ?instance ?s ?v)))


;;;ALL-SLOT-VALUES
(def-function all-slot-values (?i ?s)
   :constraint (slot ?s)
   :lisp-fun #'slot-values)

(def-relation SATISFIES-VALUE-TYPE (?i ?s ?type)
   :constraint (class ?type)
   :iff-def (not (exists ?v
                         (and (has-slot-value ?i ?s ?v)
                              (not (instance-of ?v ?type))))))

;;;;;;ACCESSORS FOR CLASSES;;;;

(def-function ALL-CLASS-SLOT-TYPES (?class ?slot)
  "returns all types which apply to slot ?slot of ?class"
   :constraint (and (slot ?slot)
                    (class ?class))
   :Lisp-fun #'(lambda (class slot)
                 (get-slot-type (get-domain-class class) slot)))
                 

(def-relation CLASS-SLOT-TYPE (?c ?slot ?type)
   "This relation is satisfied if ?type is a type constraint specified
    on the fillers of slot ?slot in ?c"
   :constraint (and (slot ?slot)
                    (class ?class))
   :sufficient (member ?type (all-class-slot-types ?c ?slot)))

 
(def-function THE-CLASS-MIN-SLOT-CARDINALITY  (?c ?slot) -> ?n
  :constraint (and (slot ?slot)
                    (class ?class)
                    (integer ?n))
  :lisp-fun #'(lambda (class slot)
                (or (get-min-cardinality (get-domain-class class) slot)
                    0)))


(def-relation CLASS-MIN-SLOT-CARDINALITY (?c ?slot ?N)
   "This relation is satisfied if ?n is the min-cardinality of 
    slot ?slot in ?c"
   :iff-def (= ?n (the-class-min-slot-cardinality  ?c ?slot)))


(def-function THE-CLASS-MAX-SLOT-CARDINALITY  (?c ?slot) -> ?n
  :constraint (and (slot ?slot)
                    (class ?class)
                    (integer ?n))
  :lisp-fun #'(lambda (class slot)
                (or (get-max-cardinality (get-domain-class class) slot)
                    :nothing)))

(def-relation CLASS-MAX-SLOT-CARDINALITY (?c ?slot ?N)
   "This relation is satisfied if ?n is the max-cardinality of 
    slot ?slot in ?c"
   :iff-def (and (= ?n (the-class-max-slot-cardinality  ?c ?slot))
                 (integer ?n)))

(def-function VALUE-CARDINALITY (?instance ?binary-relation) -> ?n
  "The VALUE-CARDINALITY of a binary-relation with respect to a given
   domain instance is the number of range-elements to which the rel
   maps the domain-element."
  :body (cardinality (setofall ?y
                               (holds ?binary-relation ?instance ?y)))

  :constraint (and (binary-relation ?binary-relation)
                   (non-negative-integer ?n)))

(def-relation CAN-HAVE-ONE (?instance ?binary-relation)
  "A domain instance i CAN-HAVE-ONE value for a slot R if there
   is at most 1 value v for which R(i,v) holds.
   Asserting (CAN-HAVE-ONE ?i R) in the definition of some class C,
   where ?i is the instance variable for that class, is another way
   of specifying that C is a domain restriction of R and R is
   a single-valued-slot on C."

  :iff-def (has-at-most ?instance ?binary-relation 1))

(def-relation HAS-AT-MOST (?instance ?binary-relation ?n)
  "A binary relation HAS-AT-LEAST n values on domain instance i if there
  exist no more than n distinct values v_j such that R(i,v_j) holds.
  When used in the definition of a class where ?i is the instance
  variable, (HAS-AT-MOST ?i R n) means that the slot R can have at
  most n values on any instances of the class."

  :iff-def (and (binary-relation ?binary-relation)
                (integer ?n)
                (=< (value-cardinality ?instance ?binary-relation) ?n)))

  
(def-relation HAS-ONE (?instance ?binary-relation)
  "Binary relation R HAS-ONE value on domain instance i if there
exists exactly one value v such that R(i,v) holds.
  When used in the definition of a class where ?i is the instance
variable, (HAS-ONE ?i R)  means that the slot R must always have a
value, and only one value, when applied to instance of that class."

  :iff-def (and (binary-relation ?binary-relation)
                (= (value-cardinality ?instance ?binary-relation) 1)))



(def-function ALL-CLASS-SLOTS (?c)
   :constraint (class ?c)
   :lisp-fun #'(lambda (c)
                 (domain-slots (get-domain-class c))))


(def-function LOCAL-CLASS-SLOTS (?c)
   :constraint (class ?c)
   :lisp-fun #'(lambda (c)
                 (local-slots (get-domain-class c))))




(def-function ALL-CLASS-SLOT-VALUES (?c ?s) -> ?values
  "Returns all slot values for a class slot.  If both defaults
   and definitional values are specified, only definitional
   ones are returned"
   :constraint (and (class-as-relation ?c)
                    (slot-of ?s ?C))
   :lisp-fun #'all-class-slot-values)


(def-function ALL-CLASS-SLOT-LOCAL-VALUES (?c ?s) -> ?values
  "Returns all slot values for a class slot.  If both defaults
   and definitional values are specified, only definitional
   ones are returned"
   :constraint (and (class-as-relation ?c)
                    (slot-of ?s ?C))
   :lisp-fun #'all-class-slot-local-values)




(def-function THE-CLASS-SLOT-VALUE (?c ?s) -> ?value
  "Returns a slot value (usually the only one) 
   for a class slot"
   :constraint (and (class-as-relation ?c)
                    (slot-of ?s ?C))
   :body (first (all-class-slot-values ?c ?s)))

(def-procedure SET-SLOT-VALUE (?i ?s ?v)
  :constraint (and (instance-of ?i ?c)
                   (slot-of ?s ?c))
  :body (do
          (unassert (list-of ?s ?i '?any))
          (tell (list-of ?s ?i ?v))))

(def-procedure APPEND-SLOT-VALUE (?i ?s ?v)
  "This function assumes that teh filler of slot
   ?s of ?i is a list and appends a value to it.  
   This is different from add-slot-value, which adds 
   a new slot value."
  :constraint (and (instance-of ?i ?c)
                   (slot-of ?s ?c))
  :body (set-slot-value 
         ?i ?s (cons ?v (the-slot-value ?i ?s))))



(def-function classes-with-slot (?slot)
  :constraint (slot ?slot)
  :lisp-fun #'(lambda (slot)
                (mapcar #'name (slot-of (get-relation slot)))))

(def-function classes-with-local-slot (?slot)
  :constraint (slot ?slot)
  :lisp-fun #'(lambda (slot)
                (mapcar #'name 
                        (filter (slot-of (get-relation slot))
                        #'(lambda (Class)
                            (member slot (local-slots class)))))))



(def-relation SLOT-OF (?slot ?c)
   :constraint (and (slot ?slot)
                    (class ?c))
   :iff-def (member ?slot (all-class-slots ?c))
   :prove-by (or (and (variable-bound ?c)
                      (member ?slot (all-class-slots ?c)))
                 (and (not (variable-bound ?c))
                      (variable-bound ?slot)
                      (member ?c (classes-with-slot ?slot)))
                 (and (not (variable-bound ?c))
                      (not (variable-bound ?slot))
                      (slot ?slot)
                      (member ?c (classes-with-slot ?slot))))
   :no-proofs-by (:iff-def))

(def-relation LOCAL-SLOT-OF (?slot ?c)
   :constraint (and (slot ?slot)
                    (class ?c))
   :iff-def (member ?slot (local-class-slots ?c))
   :prove-by (or (and (variable-bound ?c)
                      (member ?slot (local-class-slots ?c)))
                 (and (not (variable-bound ?c))
                      (variable-bound ?slot)
                      (member ?c (classes-with-local-slot ?slot)))
                 (and (not (variable-bound ?c))
                      (not (variable-bound ?slot))
                      (slot ?slot)
                      (member ?c (classes-with-local-slot ?slot))))
   :no-proofs-by (:iff-def))

                 
                 
                      