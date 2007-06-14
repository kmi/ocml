;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


(defun find-renamed-slot-in-instance (slot instance)
  (find-renamed-slot-in-chains slot (renaming-chains 
                                     (parent-class instance))))

(defun find-renamed-slot-in-chains (slot chains)
  (let ((chain (find slot chains :test #'(lambda (x y)
                                     (member x y)))))
    (cadr (member slot chain))))

(defun find-renaming-slot-in-instance (slot instance)
  (find-renaming-slot-in-chains slot (renaming-chains 
                                      (parent-class instance))))

(defun find-renaming-slot-in-chains (slot chains)
  (let ((chain (find  slot chains :test #'(lambda (x y)
                              (member x y)))))
    (cadr (member slot (reverse chain)))))

;;;FIND-RENAMING-SUB-CHAIN - returns all the slots on the left of <slot>
;;;in the chain in which <slot> can be found
(defun find-renaming-sub-chain (slot chains)
  (let ((chain (find  slot chains :test #'(lambda (x y)
                              (member x y)))))
    (reverse(cdr (member slot (reverse chain))))))
  
;;;CONSTRUCT-RENAMING-CHAINS-FOR-CLASS
(defun construct-renaming-chains-for-class (superclasses 
                                            renaming-pairs)
  
  ;;(setf renaming-pairs  ;;lets get them from specific to generic
  ;;      (mapcar #'reverse renaming-pairs))
  (loop with chains = (apply #'append 
                             (mapcar #'(lambda (super)
                                         (renaming-chains super))
                                     superclasses))
        for pair in renaming-pairs
        for chain = (find (second pair) chains
                          :test #'(lambda (x y)
                                    (eq x (first y))))
        do
        (if chain
          (setf chains (subst (cons (first pair) chain) chain chains))
          (setf chains (cons pair chains)))
        finally
        (return chains)))

(defun remove-obsolete-renaming-pairs (renaming-pairs domain-slots)
  (remove-if #'(lambda (pair)
                 (or (not (member (car pair) domain-slots))
                     (not (member (second pair) domain-slots))))
             renaming-pairs))
        
  
;;;CHECK-LOCAL-RENAMING-FEASIBILITY          
(defun check-local-renaming-feasibility (name local-slots inherited-slots renaming-pairs)
  (loop with bad-pairs = nil
        with all-slots = (append local-slots inherited-slots)
        for pair in renaming-pairs
        do
        (unless (and (member (car pair) all-slots)
                     (member (second pair) all-slots))
          (push pair bad-pairs))
        finally
        (when bad-pairs 
          (if (cdr bad-pairs)
            (warn "Slot renaming pairs ~{~s ~} in ~s do not specify two known slots and will be ignored"
                  bad-pairs name)
            (warn "Slot renaming pair ~s in ~s does not specify two known slots and will be ignored"
                  (car bad-pairs) name))
          (setf renaming-pairs (set-difference renaming-pairs bad-pairs))))
  renaming-pairs)
                         

(defun construct-chain (chain pairs)
  (let* ((pivot (car (last chain)))
        (pair (find pivot pairs :test #'(lambda (x y)
                                              (eq (first y) x)))))
    (if pair
      (construct-chain (append chain (list (second pair))) pairs)
      chain)))


(defun find-renaming-chain (slot chains)
  (find slot chains
        :test #'(lambda (x y)
                  x ;;ignore
                  (member slot y))))

(defun collect-starting-points (pairs)
  (loop with result = nil
        for pair in pairs
        do 
        (unless (find (car pair) pairs :test #'(lambda (x y)
                                                     (eq (second y) x)))
          (push (car pair)
                result))
        finally
        (return result)))

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

(defun propagate-renaming-from-instance-slot (instance slot value chains)
  (let ((chain (find-renaming-chain slot chains)))
    (when chain
      (loop for other-slot in (remove slot chain)
            do
            ;;we use maybe-add-slot-value-to-instance just in case 
            ;;the same value is asserted for two slots in the same chain
            (maybe-add-slot-value-to-instance instance other-slot value t nil nil)))))
  
          
        
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