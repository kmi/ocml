;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

;;;Forward chaining rules are factorised into packets.  A packet is just a set of
;;;fc rules.  The packet system has no hierarchical structure.

;;;*RULE-PACKETS*
;;;;(defvar *rule-packets* nil
 ;;;; "A list of pairs (name . packet)"
 ;;; )

(defun get-rule-packet (name) 
  (find-rule-packet-in-ontology name))

(defun add-rule-packet ( packet)
  (add-rule-packet-in-ontology packet))

(defun all-rule-packets ()
  (ontology-rule-packets))

;;;REMOVE-ALL-PACKETS ---removes all packets and then initialize the packet system
;;;(defun remove-all-packets ()
;;;  (Setf *rule-packets* nil)
 ;;;; (initialize-rule-packets))


;;;RULE-PACKET
;;(defclass rule-packet (name-mixin basic-ocml-object)
;;  ((hp-rules :initform nil :accessor rule-packet-hp-rules)
;;   (np-rules :initform nil :accessor rule-packet-np-rules)
;;   (lp-rules :initform nil :accessor rule-packet-lp-rules)))



;;;CLASS RULE-PACKET
;;;If a rule packet, say p, is active, then when in watcher-mode,
;;;rule belonging to p will be compiled automatically.

(defclass rule-packet (name-mixin basic-ocml-object)
  ((hp-rules :initform nil :accessor rule-packet-hp-rules)
   (np-rules :initform nil :accessor rule-packet-np-rules)
   (lp-rules :initform nil :accessor rule-packet-lp-rules)
   (active? :initform nil :accessor rule-packet-active?)))



  
(defun get-or-create-packet (name)
  (or (find-rule-packet-in-ontology  name)
      (new-rule-packet name)))

(defun new-rule-packet (name)
  (let ((p (make-instance 'rule-packet :name name)))
    (add-rule-packet  p)
    p))

(defun find-fc-rule-in-packet (name packet)
  (let ((fun  #'(lambda (x y)
		  (equal x (name y)))))
    (or 
     (find name  (rule-packet-hp-rules packet) :test fun)
     (find name  (rule-packet-np-rules packet)  :test fun)
     (find name  (rule-packet-lp-rules packet)  :test fun))))

(defun add-fc-rule-to-packet (packet nrule order priority)
  (case priority
    (:high
     (setf (rule-packet-hp-rules packet)
           (add-fc-rule-by-order nrule order (rule-packet-hp-rules packet))))
    (:normal
     (setf (rule-packet-np-rules packet)
           (add-fc-rule-by-order nrule order (rule-packet-np-rules packet))))
    (:low
     (setf (rule-packet-lp-rules packet)
           (add-fc-rule-by-order nrule order (rule-packet-lp-rules packet))))))

(defun remove-fc-rule-from-packet (packet rule priority)
  (case priority
    (:high
     (setf (rule-packet-hp-rules packet)
           (remove rule (rule-packet-hp-rules packet))))
    (:normal
     (setf (rule-packet-np-rules packet)
           (remove rule (rule-packet-np-rules packet))))
    (:low
     (setf (rule-packet-lp-rules packet)
           (remove rule (rule-packet-lp-rules packet))))))

(defun remove-all-fc-rules-in-packet (packet)
  (setf (rule-packet-hp-rules packet) nil
        (rule-packet-np-rules packet) nil
         (rule-packet-lp-rules packet) nil))

(defun remove-fc-rules-in-all-packets ()
  (loop for packet in (ontology-rule-packets)
        do
        (remove-all-fc-rules-in-packet packet)))

;;;*DEFAULT-FC-RULE-PACKET* --The default packet where rules with no packet are
;;;sent
(defvar *default-fc-rule-packet* )


;;;(defun initialize-rule-packets ()
;;;  (setf  *default-fc-rule-packet* (make-default-rule-packet))
;;;;  (add-rule-packet (rule-packet-name *default-fc-rule-packet*) *default-fc-rule-packet*))
  
;(eval-when (eval load)
;  (unless *rule-packets*
;    (initialize-rule-packets)))


;;;GET-ALL-FC-RULES ---Returns all rules which have been defined.
(defun get-all-fc-rules ()
  (merge-rules (all-rule-packets)))

(defun get-all-rules-in-packet (packet)
  (append (rule-packet-hp-rules packet)
          (rule-packet-np-rules packet)
          (rule-packet-lp-rules packet)))

(defun merge-rules (packets)
  (loop with hp-rules
        with np-rules
        with lp-rules
        for packet in packets
        do
        (setf hp-rules (nconc hp-rules (rule-packet-hp-rules packet))
               np-rules (nconc np-rules (rule-packet-np-rules packet))
               lp-rules (nconc lp-rules (rule-packet-lp-rules packet)))
        finally
        (return (append hp-rules np-rules lp-rules))))

  
;;;RULE
(defclass rule (name-mixin basic-ocml-object)
   ((direction :accessor rule-direction)
    ))

(defun get-rule (name &optional packet)
   (or 
    (find-bc-rule name)
    (find-fc-rule name packet)))


(defun remove-all-rules (&optional no-sweat? remove-packets?)
  (remove-all-bc-rules no-sweat?) ;
  (remove-all-fc-rules no-sweat? remove-packets?))


;;;PRINT-OBJECT
(defmethod print-object ((rule rule) stream)
  (with-slots (name) rule
    (format stream "#<~S ~S>" (type-of rule) name)))


;;;*BC-RULES*
(defun add-bc-rule (name rule)
  (setf (gethash name *bc-rules*)
        rule))

(defun find-bc-rule (name)
  (gethash name *bc-rules*))

(defun remove-bc-rule (name)
  (remhash name *bc-rules*))


;;;FORWARD-RULE
(defclass forward-rule (rule)
   ((if-part :initarg :if-part :accessor if-part)
    (then-part :initarg :then-part :accessor then-part)
    (lhs-variables :accessor lhs-variables)
    (direction :initform  :forward)
    (priority :initarg :priority)
    (packet :initarg :packet :accessor rule-packet)
    (order :accessor rule-order :initarg :order)
    (instantiations :accessor instantiations :initform nil)
    (instantiation-form :accessor instantiation-form)
    (alpha-nodes                            ;;Each rule can have 0 or more alpha-nodes
     :accessor alpha-nodes :initform nil)
    (compiled? :initform nil  :accessor rule-compiled?)))


(defvar *rule-counter* 0)

(defun reset-rule-counter ()
  (Setf *rule-counter* 0))

(defun priority? (x)
  (member x *fc-priorities*))

;;;;;;(defvar *fc-rules* nil)
;
;(defvar *hp-fc-rules* nil)
;(defvar *np-fc-rules* nil)
;(defvar *lp-fc-rules* nil)

;(defun ordered-rules ()
;  (append *hp-fc-rules* *np-fc-rules* *lp-fc-rules*))

;;;FIND-FC-RULE 

(defun find-fc-rule (name &optional packet)
  (if packet
      (find-fc-rule-in-packet name packet)
      (loop for packet in (ontology-rule-packets)
        for result = (find-fc-rule-in-packet name packet)
        until result
        finally (return result))))


;;;ADD-FC-RULE
;;(defun add-fc-rule (nrule order priority packet)
;;  (add-fc-rule-to-packet packet nrule order priority))


;;;ADD-FC-RULE
;;;Modified so that the rule is automatically compiled, if 
;;;it belongs to one of the current packets. 
(defun add-fc-rule (rule order priority packet)
  (prog1 
    (add-fc-rule-to-packet packet rule order priority)
    (when (and *fc-in-watcher-mode*
               (rule-packet-active? packet ))
      (compile-fc-rule rule (if-part rule)(then-part rule)))))




(defun add-fc-rule-by-order (nrule order rules)
  (let ((pos (position-if  #'(lambda (rule)
			       (> (rule-order rule)
				  order))
                           rules)))
    (if pos
        (nconc (subseq rules 0 pos)(List nrule)(subseq rules pos))
        (append rules (list nrule)))))

;(defun add-fc-rule (rule order)
;  (if order
;      (fill *fc-rules* rule :start order :end (1+ order))
;      (setf *fc-rules* (nconc *fc-rules* (list rule)))))


;;;REMOVE-RULE FORWARD-RULE--- Top level method for removing a rule
(defmethod remove-rule ((rule forward-rule))
  (with-slots (priority packet) rule
    (clear-alpha-nodes rule)
    (remove-fc-rule-from-packet packet rule priority)))

;;;REMOVE-ALL-FC-RULES
(defun remove-all-fc-rules (&optional no-sweat? remove-packets?)
  (reset-rule-counter)
  (if remove-packets?
      (clear-all-packets)
      (remove-fc-rules-in-all-packets))
  (unless no-sweat?
    (clear-all-alpha-nodes)))


(defmethod link-alpha-nodes-to-candidates ((rule forward-rule))
  (with-slots (alpha-nodes) rule
    (dolist (alpha-node alpha-nodes)
      (link-alpha-node-to-candidates alpha-node))))

(defmethod clear-alpha-nodes ((rule forward-rule))
  (with-slots (alpha-nodes) rule
    (dolist (alpha-node alpha-nodes)
      (remove-alpha-node (alpha-node-relation alpha-node) alpha-node t)
      (dolist (relation (indirect-relations alpha-node))
        (remove-indirect-alpha-node relation alpha-node)))))


(defmethod fetch-end-node ((rule forward-rule))
  (with-slots (alpha-nodes) rule
    (when alpha-nodes
      (if (cdr alpha-nodes)
	  (rete-node-successor (rete-node-successor (car (last alpha-nodes))))
          (rete-node-successor (car alpha-nodes))))))
    

(defclass backward-rule (rule)
   ((direction :initform :backward)
    (clauses :initarg :clauses :accessor clauses)
    (defines-relation :accessor defines-relation)))

(defclass bc-clause ()
  ((consequent :initarg :consequent :reader bc-clause-consequent)
   (antecedents :initarg :antecedents :reader bc-clause-antecedents)
   (renamed-instance :accessor renamed-instance)
   (variables :initarg :variables :accessor variables)))

(defclass iff-def-clause (bc-clause)())

(defun iff-def-clause? (thing)
  (typep thing 'iff-def-clause))

(defun bc-clause? (thing)
  (typep thing 'bc-clause))

(defun def-rule-internal (name documentation args
                               &aux (priority *default-fc-rule-priority*)
                               (packet (default-fc-rule-packet)))
  (Let (pos1 pos2)
    (unless (stringp documentation)
      (setf args (cons documentation args)
            documentation nil))
    (setf pos1 (position :priority args)
          pos2 (position :packet args))
    (cond (pos1
	   (setf priority (elt args (1+ pos1)))
           (cond (pos2
                  (setf packet
                        (get-or-create-packet (elt args (1+ pos2))))
                  (setf args (subseq args (+ 2 (max pos1 pos2)))))
                 (t
                  (setf args (subseq args (+ 2 pos1))))))
	  (pos2
	   (setf packet (get-or-create-packet (elt args (1+ pos2))))
           (setf args (subseq args (+ 2 pos2))))))
  (add-canonical-rule name documentation priority packet args))

;(defun def-rule-internal (name documentation priority  args)
;  (cond ((Priority? documentation)
;         (setf  args (cons priority args)
;                priority documentation
;                documentation nil))                
;        ((stringp documentation)
;         (unless (Priority? priority)
;           (setf args (cons priority args)
;                 priority *default-fc-rule-priority*)))
;        (t
;         ;;There is no priority or documentation
;         (setf args (cons documentation (cons priority args))
;               priority *default-fc-rule-priority*
;               documentation nil)))
;  (add-canonical-rule name documentation priority args))

(defun add-canonical-rule (name documentation priority packet args)
  ;;#-:lispworks(record-source-file name 'ocml-rule)
  ;;#+(or allegro lispworks)(record-source-file name 'def-rule)

  #-:lispworks(ocml-record-source-file name 'ocml-rule)
  #+(or allegro lispworks)(ocml-record-source-file name 'def-rule)
  (let ((then-pos (position 'then args)))
    (if then-pos
	(add-forward-rule name documentation priority packet args then-pos)
	(add-backward-rule name documentation args))))

;(defun def-rule-internal (name documentation args)
;  (unless (stringp documentation)
;    (setf args (cons documentation  args)
;          documentation nil))
;  (let ((then-pos (position 'then args)))
;    (if then-pos
;	(add-forward-rule name documentation args then-pos)
;	(add-backward-rule name documentation args))))


(defun add-forward-rule (name documentation  priority packet args then-pos)
  (Let* ((if-part (subseq args 0 then-pos))
         (then-part (subseq args (1+ then-pos)))
         (old (get-rule name))
         order rule)
    (unless then-part
      (error "Rule ~S has no right hand side"name))
    (cond (old
           (ocml-warn "Redefining rule ~S" name)
           (cond ((typep old 'forward-rule)
                  (setf order (rule-order old)))
		 (t
		  (setf order *rule-counter*)
                  (incf *rule-counter*)))
           (remove-rule old))
          (t
           (setf order *rule-counter*)
           (incf *rule-counter*)))
    (setf rule
          (make-instance 'forward-rule
                         :name name
                         :priority priority
                         :packet packet
                         :if-part if-part
                         :then-part then-part
                         :documentation documentation
                         :order order))
    (add-fc-rule rule order priority packet)
    rule))


;;;ADD-BACKWARD-RULE --Adds a new backward rule.  The name does not need to be the same as the
;;;relation id
;;;Modified by mauro
(defun add-backward-rule (name documentation clauses &optional arity)
  (Let* ((old-rule (find-bc-rule name))
         (rule (make-instance 'backward-rule
                 :name name
                 :clauses (mapcar #'make-bc-rule-clause clauses)
                 :documentation documentation))
         (tail  (caar clauses))
         (rel (or (car tail) name)))
    (when old-rule
      (warn "Redefining rule ~s"name)
      (remove-rule old-rule))
    (add-bc-rule name rule)
    (setf (defines-relation rule) rel)
    (add-defined-by-rule-entry
     (find-or-create-relation rel (or arity (length (cdr tail))))
                               rule)
    (propagate-new-def-to-sub-ontologies name rule 'bc-rule)
    rule))





(defun make-bc-rule-clause (clause &optional (type 'bc-clause))
  (make-instance type
                 :consequent (car clause)
                 :antecedents (cddr clause)))
;;;                 :variables (collect-variables clause)))

(defun remove-all-bc-rules (&optional no-sweat?)
  (unless no-sweat?
    (clear-all-defined-by-rule-entries))    
  (clrhash *bc-rules*))



;;;REMOVE-RULE BACKWARD-RULE--- Top level method for removing a rule
(defmethod remove-rule ((rule backward-rule))
  (with-slots (name defines-relation) rule
    (remove-bc-rule name)
    (remove-defined-by-rule-entry (get-relation defines-relation)rule)
    (maybe-fetch-definition-from-super-ontologies *current-ontology*
                                                'ontology-bc-rules
                                                name 
                                                *current-ontology*)
    (remove-def-from-dependent-ontologies name rule 'bc-rule)))



  