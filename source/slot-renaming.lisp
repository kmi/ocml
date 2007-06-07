;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

;(defun find-renamed-slot-in-instance (slot instance)
;  (let ((slot-class-pairs (setofall '(?y ?c) `(renamed-slot ?y ,slot ?c))))
;    (first 
;     (mapcar #'(lambda (x)(first x))
;            (filter slot-class-pairs #'(lambda (pair)
;                                         (member instance
;                                                 (all-current-instances (second pair)))))))))


;(defun find-renaming-slot-in-instance (slot instance)
;  (let ((slot-class-pairs (setofall '(?y ?c) `(renamed-slot ,slot ?y ?c))))
;    (first (mapcar #'(lambda (x)(first x))
;            (filter slot-class-pairs #'(lambda (pair)
;                                         (member instance
;                                                 (all-current-instances (second pair)))))))))

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




;;;FIND-RENAMED-SLOTS-IN-CLASS - checks whether a slot is a specific name for a more a 
;;;generic slot -.e.g awarding-body is a specific name for main-agent.  Returns the renamed
;;;slot (the more generic one).
;(defun find-renamed-slots-in-class (slot class)
;  (let ((slot-class-pairs (setofall '(?y ?c) `(renamed-slot ?y ,slot ?c))))
;    (mapcar #'(lambda (x)(first x))
;            (filter slot-class-pairs #'(lambda (pair)
;                                         (subclass-of? class (get-ocml-class
;                                                             (second pair))))))))
;
;(defun construct-renaming-chains-for-class-early (class  renaming-pairs)
;  (setf renaming-pairs  ;;lets get them from specific to generic
;        (mapcar #'reverse renaming-pairs))
;  (let ((chains)
;        (triples (filter (setofall '(?gen-slot ?spec-slot ?c) 
;                                    `(renamed-slot ?gen-slot ?spec-slot ?c))
;                          #'(lambda (triple)
;                              (subclass-of? class (get-ocml-class
;                                                   (third triple)))))))
;    (when (or triples renaming-pairs)
;      (setf triples (append renaming-pairs
;                                    triples))
;      (setf chains (mapcar #'list 
;                           (collect-starting-points 
;                            triples)))
;      (loop for chain in chains
;            collecting (construct-chain chain triples)))))


(defun construct-renaming-chains-for-class (superclasses renaming-pairs)
  
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



(defun construct-chain (chain pairs)
  (let* ((pivot (car (last chain)))
        (pair (find pivot pairs :test #'(lambda (x y)
                                              (eq (first y) x)))))
    (if pair
      (construct-chain (append chain (list (second pair))) pairs)
      chain)))


;(defun construct-renaming-chains-for-class (class)
;  (let* ((chains)
;         (name (name class))
;         (triples (filter (setofall '(?gen-slot ?spec-slot ?c) 
;                                    `(renamed-slot ?gen-slot ?spec-slot ?c))
;                          #'(lambda (triple)
;                              (or (eq name
;                                      (third triple))
;                                  (subclass-of? class (get-ocml-class
;                                                       (third triple))))))))
;    (when triples
;      (setf chains (mapcar #'list 
;                           (collect-starting-points triples)))
;      (loop for chain in chains
;            collecting (construct-chain chain triples)))))

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