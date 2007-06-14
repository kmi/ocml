;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")




;;;SLOT-RENAMED? --True if <slot> appears in one of <chains> in a position other than first
(defun slot-renamed? (slot chains)
  (some #'(lambda (chain)
            (member slot (cdr chain)))
        chains))

;;;FIND-ALL-CHAINS-LED-BY-SLOT - Finds all chains starting with the given slot
(defun find-all-chains-led-by-slot (slot chains)
  (filter chains #'(lambda (c)
                     (eq  slot (first c)))))

;;;FIND-ALL-RENAMING-CHAINS-WITH-SLOT - Finds all chains in which <slot> appears
(defun find-all-renaming-chains-with-slot (slot chains)
  (remove-if-not #'(lambda (chain)
                     (member slot chain))
                 chains))

(defun all-slots-renamed-by-slot-x (slot chains)
  (remove slot 
          (find-renaming-chain-with-slot slot chains)))

;;;FIND-RENAMING-CHAIN-WITH-SLOT - FInds the one chain that contains <slot>
;;;This is used with effective renaming chains
(defun find-renaming-chain-with-slot (slot chains)
  (some #'(lambda (chain)
            (when (member slot chain)
              chain))
        chains))


;;;COLLECT-ALL-ASSOCIATED-SLOTS-IN-RENAMING-CHAINS - Collects all slots appearing in chains in 
;;;which <slot> appears
(defun collect-all-associated-slots-in-renaming-chains (slot chains)
  (remove slot  
          (remove-duplicates 
           (apply #'append 
                  (find-all-renaming-chains-with-slot slot chains)))))

;;;CONSTRUCT-RENAMING-CHAINS-FOR-CLASS
(defun construct-renaming-chains-for-class (renaming-pairs
                                            inherited-chains)
  (loop with list-of-chains = (list inherited-chains)
        for pair in renaming-pairs
        for relevant-chains = (find-all-chains-led-by-slot (second pair)
                                                           inherited-chains)
        for new-chains = (if relevant-chains
                                  (mapcar #'(lambda (c)
                                           (cons (first pair) c))
                                          relevant-chains)
                                  (List pair))
        do
        (push new-chains list-of-chains)
        finally
         (return (normalize-inherited-chains list-of-chains))))

    
;;;NORMALIZE-INHERITED-CHAINS - Takes as input a list of lists of chains
;;;and produces a list of chains where no chain is subsumed by another
;;;For instance, given an input such as 

;;;     (((a b)) ((a b c)(1 2)))

;;; it will produce 

;;;           ((a b c)(1 2))

(defun normalize-inherited-chains (list-of-chains)
  (let ((chains (remove-duplicates  
                 (apply #'append list-of-chains)
                 :test #'equal)))
    (remove-if #'(lambda (chain)
                   (some #'(lambda (chain2)
                             (search chain chain2))
                         (remove chain chains)))
               chains)))

(defun merge-renaming-chains (chains)
  (if (cdr chains)
    (let ((element (car
                    (some #'(lambda (el)
                              (some #'(lambda (chain)
                                        (member el chain))
                                    (cdr chains)))
                          (first chains)))))
      (if element
        (let ((relevant-chains (filter chains #'(lambda (chain)
                                                  (member element chain))))
              (other-chains (remove-if #'(lambda (chain)
                                           (member element chain)) 
                                       chains)))
          (merge-renaming-chains (cons (union* relevant-chains)
                                       other-chains)))
        (cons (first chains)
              (merge-renaming-chains (cdr chains)))))
    chains))
        
  
;;;CHECK-LOCAL-RENAMING-FEASIBILITY  
;;;Slot renaming is about introducing a new slot, say s-new, for a class, say C
;;;and stating that such new slot 'replaces' some inherited slot, s-old.  
;;;Logically this option amounts to stating the following axiom:

;;;(forall (?s-new ?s-old ?c)
;;;        (=> (slot-renaming ?s-new ?s-old ?c)
;;;            (and (class ?c)
;;;                 (local-slot-of ?s-new ?c)
;;;                 (slot-of ?s-old ?c)
;;;                 (not (local-slot-of ?s-old ?c))
;;;                 (forall ?c-super
;;;                         (=> (subclass-of ?c ?c-super)
;;;                             (and (not (slot-of ?s-new ?c-super))
;;;                                  (not (exists ?other-slot
;;;                                               (and (slot-of ?other-slot ?super)
;;;                                                    (slot-renaming ?other-slot ?s-old ?super)))))))
;;;                 (forall ?i
;;;                         (=> (instance-of ?i ?c)
;;;                             (and (<=> (s-new ?i ?x)(s-old ?i ?x))
;;;                                  (<=> (applicable-constraint ?i ?s-new ?constr)
;;;                                       (applicable-constraint ?i ?s-old ?constr))))))))

;;;That is, if a new slot renames an old slot in a class C, it means that for all instances of 
;;;C these 2 slots become indistinguishable:  all applicable constraints associated with the 2 slots 
;;;apply to both of them and all values derivable for either slot apply to both of them.  In addition
;;;renaming-chains are 'directed'. That is, if a class c inherits a chain (s2 s3), then a local slot 
;;;s1 can rename s2, but not s3

(defun check-local-renaming-feasibility (name local-slots inherited-slots renaming-pairs inherited-chains)
  (let* ((normalized-pairs (remove-duplicates renaming-pairs :test #'(lambda (x y)
                                                                   (eq (second x)
                                                                           (second y)))))
        (bad-pairs (set-difference renaming-pairs normalized-pairs)))
    (loop for pair in bad-pairs
          do
          (warn 
           "Slot renaming pair ~s in ~s is incorrect: renamed slot ~s appears in some other renaming pair in ~s"
           pair name (second pair) renaming-pairs))

  (let ((sound-pairs 
         (loop with bad-pairs = nil
               with all-slots = (append local-slots inherited-slots)
               for pair in normalized-pairs
               do
               (when (or  
                      (not (member (car pair) local-slots))
                      (member (car pair) inherited-slots)
                      (not (member (second pair) inherited-slots))
                      (member (second pair) local-slots)
                      (slot-renamed? (second pair)
                                     inherited-chains))
                 (push pair bad-pairs))
                 finally
                 
                 (loop for pair in bad-pairs
                       do
                       (cond ((not (and (member (car pair) all-slots)
                                        (member (second pair) all-slots)))
                              (warn "Slot renaming pair ~s in ~s does not specify two known slots and will be ignored"
                                    pair name))
                             ((or (member (car pair) inherited-slots)
                                  (member (second pair) local-slots))
                              (warn 
                               "Slot renaming pair ~s in ~s is incorrect:  either ~s is not a new slot, or ~s is not an old slot"
                               pair name (car pair) (second pair)))
                             ((slot-renamed? (second pair)
                                             inherited-chains)
                              (warn 
                               "Slot renaming pair ~s in ~s is incorrect: ~s is already renamed by some other slot in the inherited chains ~s"
                               pair name (second pair) inherited-chains))
                             (t
                              (warn 
                               "Sorry, slot renaming pair ~s in ~s is incorrect, but I don't really know why..."
                               pair name))))
                 (return 
                  (set-difference normalized-pairs bad-pairs)))))
    (check-indirect-local-slot-renaming name local-slots inherited-slots sound-pairs 
                                        (merge-renaming-chains inherited-chains)))))

(defun check-indirect-local-slot-renaming (name local-slots inherited-slots renaming-pairs chains)
  (if (cdr renaming-pairs)
    (Let* ((pair (car renaming-pairs))
           (o-pair (some #'(lambda (x)
                             (when (intersection
                                    (all-slots-renamed-by-slot-x (second pair) chains)
                                    (all-slots-renamed-by-slot-x (second x) chains))
                               x))
                         (cdr renaming-pairs))))
      (cond (o-pair
             (warn 
              "Slot renaming pair ~a in ~a is redundant: it renames the same set of slots, ~s, as ~a.  ~a will be removed"
              pair name (all-slots-renamed-by-slot-x (second pair) chains) o-pair pair)
             (check-indirect-local-slot-renaming name local-slots inherited-slots (cdr renaming-pairs) chains))
            (t
             (cons (car renaming-pairs)
                   (check-indirect-local-slot-renaming name local-slots inherited-slots (cdr renaming-pairs) chains)))))
    renaming-pairs))

                         


;;;CHECK-RENAMING-IN-INSTANCE-SLOT-VALUES
;;;This is called after an instance is created or an ancestor class has been redefined
;;;to check that all slot values inferred by renaming are correctly propagated
(defun check-renaming-in-instance-slot-values (instance parent)
  (loop with chains = (renaming-chains parent)
        for slot in (domain-slots instance)
        do
        (destructuring-bind (&optional all-values local-values flag values-inferred-by-renaming)
                            (slot-value instance slot)
          flag all-values ;;ignore
          (loop for value in local-values 
                do
                (unless (inferred-by-renaming? value values-inferred-by-renaming)
                  (propagate-renaming-from-instance-slot instance slot value chains))))))

(defun inferred-by-renaming? (value values-inferred-by-renaming)
  (member value values-inferred-by-renaming :test #'equal))


;;(defun propagate-renaming-from-instance-slot (instance slot value chains)
;;  (let ((chain (find-renaming-chain slot chains)))
;;    (when chain
;;      (loop for other-slot in (remove slot chain)
;;            do
;;            ;;we use maybe-add-slot-value-to-instance just in case 
;;            ;;the same value is asserted for two slots in the same chain
;;            (maybe-add-slot-value-to-instance instance other-slot value t nil nil)))))


(defun propagate-renaming-from-instance-slot (instance slot value chains)
  (loop for chain in (find-all-renaming-chains-with-slot slot chains)
        do
        (loop for other-slot in (remove slot chain)
            do
            ;;we use maybe-add-slot-value-to-instance just in case 
            ;;the same value is asserted for two slots in the same chain
            (maybe-add-slot-value-to-instance instance other-slot value t nil nil))))
  
          
        
(defun add-rules-associated-with-slot-renaming (name )
  ;;;add  slot-renaming pairs and associated rules  
  ;;;;;(setf (renaming-pairs (get-ocml-class name))
  ;;;;;;       renaming-pairs)        
  (loop for exp in (renaming-pairs (get-ocml-class name))
          do
          (add-renamed-slot-inference-rules name (first exp) (second exp))))

(defun remove-existing-slot-renaming-pairs (class)
  (loop for pair in (renaming-pairs class)
        do
        (remove-rule (find-bc-rule
                      (make-renamed-slot-inference-rule-name  
                       (name class) (first pair)(second pair)))))
  (setf (renaming-pairs class)
        nil))

(defun add-renamed-slot-inference-rules (class-name renaming-slot renamed-slot)
  (add-backward-rule (make-renamed-slot-inference-rule-name  class-name renaming-slot renamed-slot)
                     "Automatically generated to handle slot renaming"
                     `(((,renaming-slot ?x ?y) if (,class-name ?x) (,renamed-slot ?x ?y))))
  )

(defun make-renamed-slot-inference-rule-name (class-name renaming-slot renamed-slot)
  (read-from-string
   (string-append "IN-" (string class-name) "-" (string renaming-slot) "-RENAMES-"
                  (string renamed-slot))))