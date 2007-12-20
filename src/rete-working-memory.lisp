;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


;;;The functions in this file handle adding and removing new alpha patterns to the rete network.
;;;The top level entry points in the code for doing this are the following:

;;;MATCH-ALPHA-NODE-AGAINST-INSTANCE ---This is called in order to match
;;;a generic instance spec pattern such as (<class> <x> <attr1> <y1>.............<attrn> <yn>)
;;;to an instance of class <class>.  

;;;MATCH-ALPHA-NODE-AGAINST-POSSIBLE-INSTANCE ---This is called after new slot values have
;;;been asserted,  say value1....valuen of slot1 of inst1, to try and match a rule antecedent,
;;;with the form (<class> <instance-id> ....slot1 <value>....) to a
;;;;to a (possible) instance of class <class>.  


;;;RUN-ALPHA-TEST ---  This is called when a new relation instance (wm-pattern) has been asserted.
;;;It first checks whether the wm-pattern satisfies the infra-element
;;;condition (alpha test).  If this is the case, it propagates the pattern
;;;through the rete network.


;;;DUMMY-INPUT  ---This is called when a rule has been compiled which hasn't got any
;;;positive antecedent.  The function creates a dummy input, corresponding to a
;;;an instantiation with no LHS

;;;REMOVE-WM-PATTERN-FROM-RETE ---Top level function called when a pattern needs to be
;;;removed from the rete network.  It is the inverse of run-alpha-test

;;;MAYBE-REMOVE-INSTANCE-SPEC-INPUTS ---This is called after a number of values of
;;;the slot of an instance, say sloti, have been removed, to delete any associated alpha 
;;;alpha-input, if any exists, from a node such as (<class> <id> .........<sloti> <value>...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;MATCH-ALPHA-NODE-AGAINST-INSTANCE ---This takes care of performing the alpha node
;;;test on a generic instance spec pattern such as
;;; (<class> <x> <attr1> <y1>.............<attrn> <yn>)
(defmethod match-alpha-node-against-instance ((node alpha-node)(inst basic-domain-class))
  (with-slots (pattern successor beta-node-like?) node
    (maybe-propagate-alpha-inputs pattern successor beta-node-like?
                                  (match-spec-against-instance inst (car pattern) (cdr pattern) nil))))

(defun maybe-propagate-alpha-inputs (pattern successor  beta-node-like? envs)
  (let ((alpha-inputs (unless (eq envs :fail)
			(loop for env in envs
			      collecting (generate-instance-pattern pattern env)))))
    (when alpha-inputs
      (propagate-alpha-inputs successor alpha-inputs beta-node-like?))))

(defun propagate-alpha-inputs (successor alpha-inputs beta-node-like?)
  (if beta-node-like?                      ;Can only be beta-node-like? if it is positive
      (new-beta-inputs successor
                       (mapcar #'list alpha-inputs))
      (dolist (input alpha-inputs)
	(new-alpha-input successor input))))


;;;MATCH-ALPHA-NODE-AGAINST-POSSIBLE-INSTANCE ---This is called after new slot values have
;;;been asserted,  say value1....valuen of slot1 of inst1, to try and match a rule antecedent,
;;;with the form (<class> <instance-id> ....slot1 <value>....)
(defmethod match-alpha-node-against-possible-instance ((node alpha-node) name slot values &optional remove?)
  (with-slots (relation pattern successor beta-node-like?) node
    (when (instance-of? name                      ;First, check this is an instance of the class
			(name relation))          ;associated with the node
      (let ((env (match name (car pattern))))     ;Then, match name against instance id in instance spec
	(unless (eq env :fail)
          (if remove?
              (maybe-remove-instance-spec-inputs node name slot values)
              (maybe-propagate-alpha-inputs           ;If we succeed match the rest of the slot spec
               pattern successor beta-node-like?      ;and - if successful - propagate alpha inputs
	       (match-slot-value-spec-against-alpha-node name (name relation) slot values (cdr pattern) env))))))))

	    
;;; MATCH-SLOT-VALUE-SPEC-AGAINST-ALPHA-NODE ---This is used by
;;;match-alpha-node-against-possible-instance (which is called after new slot values have been
;;;asserted, say value1....valuen of slot1 of inst1) to try and match the slot specification
;;;part - which has the format (slot1 <value1>...........slotn <valuen>) - of an antecedent.
(defun match-slot-value-spec-against-alpha-node (name  class-name slot values all-slots-spec env 
                                                       &aux new-envs)
  (multiple-value-bind (modified-pattern the-slot-spec) ;First, remove slot pair from pattern
      (remove-slot-entry all-slots-spec slot)
    (setf new-envs
	  (if modified-pattern
              (match-spec-against-instance-slots  ;Then, match instance to modified pattern
		     (find-current-instance name class-name)
		     modified-pattern
		     env)
              '(nil)))
    (unless (eq new-envs :fail)                   ;If we succeed
	(loop with result
	      for value in values
	      for more-envs = (match* (list slot value) ;we try matching the new values as well
                                      the-slot-spec    
				      new-envs)
	      unless (eq more-envs :fail)
	      do
	      (setf result (nconc result more-envs))
              finally
              (return result)))))
               
(defun remove-slot-entry (slot-spec slot)
  (let ((position (position slot slot-spec)))
    (values
     (append (subseq slot-spec 0 position)
             (subseq slot-spec (+ 2 position)))
     (subseq slot-spec position (+ 2 position)))))
         
      
(defun generate-instance-pattern (pattern env)
  (instantiate pattern env))

;;;RUN-ALPHA-TEST ---  This is called when a new wm-pattern has been asserted.
;;;It first checks whether the wm-pattern satisfies the infra-element
;;;condition (alpha test).  If this is the case, it propagates the pattern
;;;through the rete network.
(defmethod run-alpha-test ((node alpha-node)args)
  (with-slots (test-fun successor beta-node-like?) node
    (when (funcall test-fun  args)
      (if beta-node-like?           ;Can only be beta-node-like? if it is positive
	  (new-beta-inputs successor (list (list args)))
	  (new-alpha-input successor args)))))

;;;DUMMY-INPUT  ---This is called when a rule has been compiled which hasn't got any
;;;positive antecedent.  The function creates a dummy input, corresponding to a
;;;an instantiation with no LHS
(defmethod dummy-input ((node alpha-node))
  (with-slots (successor) node
    (new-beta-inputs successor
		     (list (make-dummy-support)))))



;;;NEW-ALPHA-INPUT ---This is called when a new alpha input (i.e. a new wm pattern
;;;which has passed the relevant alpha test) is passed on to a beta node. In this case
;;;we run the beta test to check which test cases (beta inputs + new alpha input)
;;;pass the beta test.  The winners are then propagated down the rete network.
(defmethod  new-alpha-input ((node beta-node) args)
  (with-slots (alpha-inputs successor not-nodep beta-inputs) node
    (push args alpha-inputs)
    (Let* ((result (if not-nodep
                       (apply-negative-beta-filter    ;;In this case we want to collect the losers
                        node beta-inputs (list args)t)
                       (apply-positive-beta-filter node beta-inputs (list args))))
           (fun (if not-nodep #'remove-beta-inputs #'new-beta-inputs)))
      (when result
        (funcall fun successor result)))))


;;;NEW-ALPHA-INPUT END-NODE  --This handles the extreme case in which a rule
;;;has no beta nodes.  This means there is only one positive  antecedent and
;;;no negative ones.
(defmethod new-alpha-input ((node end-node) args)
  (new-beta-inputs node (list args)))




;;;NEW-BETA-INPUTS BETA-NODE ---This is called when new winners have been propagated down the network,
;;;to filter them using the beta test.
(defmethod new-beta-inputs ((node beta-node)new-beta-inputs)
  (with-slots (beta-inputs successor alpha-inputs  not-nodep) node
    (setf beta-inputs
          (append  beta-inputs new-beta-inputs))
    (let ((winners
           (if not-nodep
               (apply-negative-beta-filter node new-beta-inputs alpha-inputs)
               (apply-positive-beta-filter node new-beta-inputs alpha-inputs))))
      (when winners
	(new-beta-inputs successor winners)))))


;;;NEW-BETA-INPUTS END-NODE
(defmethod new-beta-inputs ((node end-node)inputs)
  (with-slots (beta-inputs rule) node
    (setf beta-inputs
          (nconc  beta-inputs inputs))
;;;    (when *interpreter-running*
    (unless *compiling-fc-rule*
      (new-instantiations rule inputs))))

;;;APPLY-POSITIVE-BETA-FILTER
(defmethod apply-positive-beta-filter ((node beta-node)beta-inputs alpha-inputs)
  (with-slots (test-fun) node
    (loop with winners
	  for alpha-input in alpha-inputs
	  do
	  (setf alpha-input (list alpha-input))
	  (loop for beta-input in beta-inputs
	        for test-case = (append beta-input alpha-input)
	        do
              ;;;(pprint test-fun)
                ;;;;;(pprint test-case)
	        (when (funcall test-fun test-case)		             
		  (push  test-case  winners)))
	  finally
          (return winners))))

;;;APPLY-NEGATIVE-BETA-FILTER  ---This is called to perform the beta test on a
;;;negative beta node.  It takes a set of beta inputs and a set of alpha inputs
;;;and returns all the beta inputs which have not been ruled out by any of the alpha inputs
(defmethod apply-negative-beta-filter ((node beta-node) beta-inputs alpha-inputs &optional return-losers?)
  (with-slots (test-fun index) node
    (loop with place-holder = (make-list index) ;;This is to get the test case of the right length
          for beta-input in beta-inputs
	  with winners = beta-inputs
	  while winners
	  do
          ;;;(pprint test-fun)
          
	  (loop for alpha-input in alpha-inputs
	        for test-case = (append beta-input place-holder (list alpha-input))
	        do
                ;;(pprint test-case)
	        (when (funcall test-fun test-case)
		  (setf winners (remove beta-input winners :test #'equal)))
	        while winners)
	  finally
          (return
           (if return-losers?
               (set-difference beta-inputs winners)
               winners)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;REMOVE-WM-PATTERN-FROM-RETE ---Top level function called when a pattern needs to be
;;;removed from the rete network
(defmethod remove-wm-pattern-from-rete ((node alpha-node) args)
  (with-slots (beta-node-like? successor position) node
    (if beta-node-like?
        (remove-beta-inputs-with-pattern  successor args  position) ;;;;(list (list args)))  
        (remove-alpha-input successor args  position))))

;;;MAYBE-REMOVE-INSTANCE-SPEC-INPUTS ---This is called after a number of values of
;;;the slot of an instance, say sloti, have been removed, to delete any associated alpha 
;;;alpha-input, if any exists, from a node such as (<class> <id> .........<sloti> <value>...)
(defmethod maybe-remove-instance-spec-inputs ((node alpha-node) name &optional slot values)
  (with-slots (beta-node-like? successor position) node
    (if beta-node-like?
        (if values
	    (remove-beta-inputs-with-slot-values  successor name slot values   position)
            (remove-beta-inputs-with-instance-name  successor name   position))
        (if values
	    (remove-alpha-input-with-slot-values  successor name slot values   position)
            (remove-alpha-input-with-instance-name  successor name    position)))))


;;;REMOVE-ALPHA-INPUT ---
(defmethod remove-alpha-input ((node beta-node) args position &aux temp)
  (with-slots (alpha-inputs successor not-nodep ) node
    (setf temp (remove args alpha-inputs :test #'equal))
    (unless (equal temp alpha-inputs)
      (setf alpha-inputs temp)
      (if not-nodep
        (maybe-resuscitate-beta-inputs node)
        (remove-beta-inputs-with-pattern successor args position)))))

;;;REMOVE-ALPHA-INPUT-WITH-SLOT-VALUES
(defmethod remove-alpha-input-with-slot-values ((node beta-node) name slot values  position &aux temp)
  (with-slots (alpha-inputs successor not-nodep ) node
    (setf temp (remove-if #'(lambda (input)
			      (match-slot-values input name slot values))
			  alpha-inputs))
    (unless (equal temp alpha-inputs)
      (setf alpha-inputs temp)
      (if not-nodep
          (maybe-resuscitate-beta-inputs node)
          (remove-beta-inputs-with-slot-values successor name slot values  position)))))

(defmethod remove-alpha-input-with-instance-name ((node beta-node) name position &aux temp)
  (with-slots (alpha-inputs successor not-nodep ) node
    (setf temp (remove-if #'(lambda (input)
			      (equal (car input) name))
			  alpha-inputs))
    (unless (equal temp alpha-inputs)
      (setf alpha-inputs temp)
      (if not-nodep
          (maybe-resuscitate-beta-inputs node)
          (remove-beta-inputs-with-instance-name successor name position)))))

;;;MAYBE-RESUSCITATE-BETA-INPUTS  ---This is called after a negative alpha input has been
;;;removed, to check whether any new winners can be generated.  
(defmethod maybe-resuscitate-beta-inputs ((node beta-node))
  (with-slots (beta-inputs alpha-inputs successor) node
   (let ((new-winners (set-difference
                       (apply-negative-beta-filter      ;This returns all the
                        node beta-inputs alpha-inputs)  ;current winners
                       (beta-inputs successor)
                       :test #'equal)))
     (when new-winners
       (new-beta-inputs successor new-winners)))))


;;;REMOVE-BETA-INPUTS--      
(defmethod remove-beta-inputs :before ((node beta-node)losers)
  (with-slots (beta-inputs)node
    (setf beta-inputs (set-difference beta-inputs losers :test #'equal))))
                                      

(defmethod remove-beta-inputs  ((node beta-node)losers)
  (with-slots (successor not-nodep) node
    (if not-nodep
        (remove-beta-inputs successor losers)
        (remove-beta-inputs-with-partial-set successor losers))))

(defmethod remove-beta-inputs ((node end-node) losers)
  (with-slots (rule) node
   ;; (when *interpreter-running*
     (unless *compiling-fc-rule*
      (remove-instantiations rule losers))))

;;;REMOVE-BETA-INPUTS-WITH-PATTERN ---This is called when a wm-element associated with a
;;;positive alpha node has been deleted
(defmethod remove-beta-inputs-with-pattern ((node beta-node) pattern position)
  (with-slots (beta-inputs) node
  (let ((to-be-removed (filter beta-inputs #'(lambda (x)
                                               (equal (elt x position)pattern)))))
    (when to-be-removed
      (remove-beta-inputs node to-be-removed)))))


(defmethod remove-beta-inputs-with-slot-values ((node beta-node) name slot values  position)
  (with-slots (beta-inputs) node
  (let ((to-be-removed (filter beta-inputs #'(lambda (x)
                                               (match-slot-values (elt x position) name slot values)))))
    (when to-be-removed
      (remove-beta-inputs node to-be-removed)))))


(defmethod remove-beta-inputs-with-instance-name ((node beta-node) name position)
  (with-slots (beta-inputs) node
  (let ((to-be-removed (filter beta-inputs #'(lambda (x)
                                               (equal (car (elt x position)) name)))))
    (when to-be-removed
      (remove-beta-inputs node to-be-removed)))))

(defun match-slot-values (instantiated-pattern name slot values &aux pos)
  (and (equal (car instantiated-pattern)
           name)
       (setf pos (position slot instantiated-pattern))
       (member (elt instantiated-pattern (1+ pos)) values :test #'equal)))
               

;;;REMOVE-BETA-INPUTS-WITH-PARTIAL-SET
(defmethod remove-beta-inputs-with-partial-set ((node beta-node) partial-support-sets)
  (with-slots (beta-inputs) node
    (let ((to-be-removed (filter beta-inputs #'(lambda (b)
                                                 (some #'(lambda (s)
                                                           (equal s (subseq b 0 (length s)))) 
                                                       partial-support-sets)))))
      (when to-be-removed
        (remove-beta-inputs node to-be-removed)))))





