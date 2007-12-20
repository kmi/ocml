;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package ocml)



;;;RETE-NODE ---There are two types of nodes: alpha-nodes and beta-nodes.  They both have
;;;a successor node (the next one in the RETE network) and an associated test function.
(defclass rete-node ()
  ((test-fun :accessor rete-node-test-fun :initarg :test-fun)
   (successor :accessor rete-node-successor)
   (not-nodep :accessor not-nodep :initform nil :initarg :not-nodep)))


;;;ALPHA-NODE --An alpha-node is a node performing infra-element tests.  An infra element test
;;;checks whether a wm-element matches an antecedent of a rule.
;;;For instance, the lambda expression
;;
;;;(LAMBDA (ARGS)
;;;  (AND (= (LENGTH ARGS) 4)
;;;       (MULTI-EQUAL (SAFE-ELT (SAFE-ELT ARGS 2) 0)
;;;                    (SAFE-ELT ARGS 1)
;;;                    (SAFE-ELT ARGS 0))
;;;       (MULTI-EQ (QUOTE C) (SAFE-ELT ARGS 3))))
;;;
;;;tests whether a wm-element has the form (<pred> (?x ?X (?x  ?y)c))

(defclass alpha-node (rete-node)
  ((beta-node-like? :accessor beta-node-like?
                    :initform nil)
   (relation :initarg :relation
             :accessor alpha-node-relation)
   (indirect-relations
     :initform nil
    :accessor indirect-relations)
   (position  :accessor  alpha-node-position  ;;This only applies to positive antecedents.
              :initform nil)                  ;;In negative ones it is NIL
   (pattern :accessor alpha-node-pattern)))               


;;;CONSTRUCT-ALPHA-NODE ---Constructs an alpha node and links it to the relevant
;;;predicate in the predicates' hash table
(defun construct-alpha-node (predicate args not-node?)
  (let* ((length (length args))
	 (map (construct-infra-element-map args length))
	 (relation (find-or-create-relation predicate (length args)))
         (type (get-relation-type relation))
         (node
          (make-instance 'alpha-node
                         :relation relation
                         ;;;;;:test-fun test-fun
                         :not-nodep not-node?)))
    (cond ((eq type :class)
           (setf (alpha-node-pattern node)
                 args)
           (do-indirect-fc-nodes-links node args))
          (t
           (setf (rete-node-test-fun node)
                 (construct-alpha-node-fun map  length))))
    (add-alpha-node-to-relation relation node type)
    (values node map)))

(defun do-indirect-fc-nodes-links (node pattern)
  (loop for slot in (cdr pattern) by #'cddr
        for relation = (find-or-create-relation slot 2)
        do
        (push relation (indirect-relations node))
        (add-indirect-fc-node relation node)))


;(defun construct-alpha-node (predicate args not-node?)
;  (let* ((length (length args))
;	 (map (construct-infra-element-map args length))
;         (test-fun (construct-alpha-node-fun map  length))
;         (relation (find-or-create-relation predicate (length args)))
;         (node
;          (make-instance 'alpha-node
;                         :relation relation
;                         :test-fun test-fun
;                         :not-nodep not-node?)))
;    (add-alpha-node-to-relation relation node)
;    (values node map)))

;;;CONSTRUCT-DUMMY-ALPHA-NODE ---A dummy alpha node is a place holder
;;;for the case in which we have 0 positive antecedents and n (with n>0)
;;;negative ones.  A dummy alpha node is not linked to any relation
(defun  construct-dummy-alpha-node ()
  (make-instance 'alpha-node :relation nil))

;;;SET-ALPHA-NODES-POSITIONS
(defun set-alpha-nodes-positions (nodes)
  (dotimes (i (length nodes))
    (setf (alpha-node-position (elt nodes i))i)))

;;;LINK-ALPHA-NODE-TO-CANDIDATES  ---This is called when an alpha-node is
;;;created, to link it to the relevant candidates
(defmethod link-alpha-node-to-candidates ((node alpha-node))
  (with-slots (relation) node
    (when relation
      (multiple-value-bind (candidates flag)
          (find-alpha-node-candidates relation)
        (when candidates
          (case flag
            (:relation-instances
             (dolist (inst candidates)
               (run-alpha-test node (args inst))))
            (:slot
             (dolist (inst candidates)
               (Let ((values (get-slot-values inst (name relation)))
                     (name (name inst)))
                 (dolist (value values)
		   (run-alpha-test node (list name value))))))
            (:class
             (dolist (inst candidates)
               (match-alpha-node-against-instance node inst)))))))))

  

;;;CONSTRUCT-ALPHA-NODE-FUN ---
;;;This generates the infra-element test condition, associated with an alpha node
(defun construct-alpha-node-fun (map length &aux var-addresses const-addresses result)
  (loop for pair in map
        do
        (cond ((variable? (car pair))
               (when (cddr pair)
                 (push (cdr pair)
                       var-addresses)))
              (t
               (Push pair const-addresses))))
  (setf result  (eval
                 `(function (lambda (args)
                              (and (= (length args) ,length)
                                   ,@(or (generate-multi-equality-tests     
                                          ;if var-addresses = nil
                                          ;we have a trivial test
                                          var-addresses 'args)     
                                         '(t))                               
                                   ,@(or
                                      (generate-multi-equality-tests 
                                       const-addresses 'args
                                       #'generate-multi-eq-test)
                                      '(t)))))))
  #+:lispworks
  (compile        ;;;In mcl & allegro eval = compile, so no need to compile
   (gensym)       
   result)   
  #-:lispworks
  (eval result))
 

;(defun construct-alpha-node-fun (map length &aux var-addresses const-addresses)
;  (loop for pair in map
;        do
;        (cond ((variable? (car pair))
;               (when (cddr pair)
;                 (push (cdr pair)
;                       var-addresses)))
;              (t
;               (Push pair const-addresses))))
;  (compile
;   (gensym)
;   (eval
;   `(function (lambda (args)
;               (and (= (length args) ,length)
;                    ,@(or (generate-multi-equality-tests     ;if var-addresses = nil
;                           var-addresses 'args)               ;we have a trivial test
;                          '(t))                               
;                    ,@(or
;                       (generate-multi-equality-tests const-addresses 'args
;                                                      #'generate-multi-eq-test)
;                       '(t))))))))

;;;CONSTRUCT-INFRA-ELEMENT-MAP 
(defun construct-infra-element-map (args length)
  (loop with map
        for el in args
        for i from 0 to length
        do
        (setf map (new-arg-position el map (list i)))
        finally
        (return map)))

;;;NEW-ARG-POSITION
(defun new-arg-position (el map address)
  (cond ((atom el)
	 (if (assoc el map)
             (setf (right-value el map) (cons address (right-value el map)))
             (setf map (acons el (list address) map))))
        (t
         (loop for subel in el
               for i from 0 to (length el)
               do
	       (setf map (new-arg-position subel map (cons i address))))))
  map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;BETA-NODE ---
(defclass beta-node (rete-node)
  ((alpha-inputs :accessor alpha-inputs :initform nil)
   (beta-inputs :accessor beta-inputs :initform nil)
   (index :initarg :index)))




;;;END-NODE
(defclass end-node (beta-node)
  ((rule :accessor end-node-rule :initarg :rule)))



;;;CONSTRUCT-BETA-NODES ---Constructs the beta nodes associated with a rule and
;;;returns the last one
(defun construct-beta-nodes (rule nodes map index length predecessor not-nodes-index)
  (if (< index length)
    (let ((beta-node
           (construct-a-beta-node (elt nodes index) map index predecessor  not-nodes-index )))
      (construct-beta-nodes
       rule
	   nodes
	   map
	   (1+ index)
           length
           beta-node
           not-nodes-index
           ))
    (setf (rete-node-successor predecessor)
          (make-instance 'end-node :rule rule))))


;;;CONSTRUCT-A-BETA-NODE ---Creates a beta node taking care of linking it to its
;;;predecessors in the RETE network.
;;;<index> indicates the position of <alpha-node> in the antecedent of the rule.
(defun construct-a-beta-node (alpha-node map index beta-node-predecessor not-nodes-index )
  (let*((not-nodep (not-nodep alpha-node))
        (beta-node (make-instance 'beta-node
                                  :not-nodep not-nodep
                                  :index (when not-nodep
                                           (- index not-nodes-index))))
        (tests (mapcan #'(lambda (var-entry)
                           (maybe-generate-beta-test var-entry index not-nodes-index))
                       map)))
    (setf 
          (rete-node-successor alpha-node) beta-node
          (rete-node-successor beta-node-predecessor) beta-node
          (rete-node-test-fun beta-node) (make-beta-node-test-fun tests))
    beta-node))

(defun make-beta-node-test-fun (tests &aux result)
  #-:lispworks (setf result `(function
		              (lambda (patterns)
		                (and ,@tests))))
  #+:lispworks (setf result ` (lambda (patterns)
		                (and ,@tests)))
  #+:lispworks
  (compile       ;;;In mcl & allegro eval = compile.  
   (gensym)       
   result)  
  #-:lispworks
  (eval result))


  

;;;MAKE-INTER-ELEMENTS-MAP ---The inter-elements-map is produced by 'merging'
;;;the various infra-element-maps associated with the antecedents in the LHS.
;;;Each infra-element-map has the format (<entry> .........<entry>),
;;;and each entry has the format (<var-or-const> . (<address1>...........<addressn>))
;;;We want to merge them to produce a inter-elements map, which contains entries such as
;;;(<var> . (<naddress1>...........<naddressn>)), where <var> occurs in at least
;;;2 infra-element maps, and <naddress> includes the index of the antecedent.
(defun make-inter-elements-map (maps)
  (Loop with inter-elements-map
        with vars = (collect-vars maps)   ;Collect all vars appearing in more than 1 antecedent
        for var in vars
        do
        (push (construct-inter-elements-map-entry
               var maps)
              inter-elements-map)
        finally
        (return inter-elements-map)))

;;;CONSTRUCT-INTER-ELEMENTS-MAP-ENTRY ---Generates the entry in the inter-elements-map
;;;associated with a specific variable. For instance, given the LHS
;;;
;;; ((foo ?x ?x c)(poo ?X ?X ?y) (bar ?x ?Z)(foo a a (?X)?Z))
;;;
;;;the entry in the inter-elements-map for ?z is
;;;
;;;(?Z NIL NIL (1 2) (3 3))
;;;
;;;This entry shows that ?z appears in the third and fourth antecedents.

(defun construct-inter-elements-map-entry (var maps)
  (loop with addresses
        ;;addresses is going to store the address
        ;;of this variable in each antecedent,
        ;;or NIL if the variable doesn't appear
        ;;in an antecedent
        for map in maps                       ;maps are infra-element-maps
        for i from 0                          ;i = index of the antecedent associated with map
        do
        (if 
            (assoc var map)                   ;if var appears in the map for this antecedent (i)
	    (push (append                      ;then we augment the address to include also
		   (car (right-value          ;the reference to the antecedent and we store the
                         var map))            ;newly-formed inter-elements address in addresses
	           (list i))                  ;Otherwise we put NIL in addresses.
                  addresses)
            (push nil addresses))
        finally
        (return (cons var (nreverse addresses)))))
    
;;;COLLECT-DUPLICATE-VARS ---Collects all variables appearing in more than one
;;;infra-element-map.
;;;The algorithm is the following.  We scan all the entries in each map, collecting all
;;;occurrences of a variable in <copy> and producing a list of all variables, with no duplications
;;;(in <vars>).  Then we remove 1 occurrence of each variable in <copy>. <copy> now contains
;;;only variables which appear in more than 1 map.  Therefore, we return (remove-duplicates copy).
;;;I'm sure there is probably a much better way of doing this.
(defun collect-duplicate-vars (maps &aux vars copy)
  (loop for map in maps
        do
        (loop for pair in map
              when (variable? (car pair))
              do
              (pushnew (car pair) vars)
              (push (car pair) copy)))
  (loop for var in vars
	do
        (setf copy (remove var copy :count 1))
        finally (return (remove-duplicates copy))))

;;;COLLECT-VARS ---Collects all variables appearing in the left hand side of a rule
(defun collect-vars (maps &aux vars)
  (loop for map in maps
        do
        (loop for pair in map
              when (variable? (car pair))
              do
              (pushnew (car pair) vars)))
  vars)

;;;MAYBE-GENERATE-BETA-TEST ---Takes as args an entry in the inter-elements-maps and
;;;an index, corresponding to the 'level' of the beta-node in the network (e.g first
;;;beta-node is at level 1 and tests compatibility of wm-element matching antecedent 0 and
;;;wm-element matching antecedent 1).  The function generates a test if the variable is
;;;shared by at least two antecedents at level which is less than <index>.
(defun maybe-generate-beta-test (var-entry index not-nodes-index)
  (let* ((addresses (cdr var-entry))
         (this-node-entry (elt addresses index)))
    (when this-node-entry
      (loop for i from (1- index) downto 0
            for address = (elt addresses i)
            do
            (when (and address
                       (< i not-nodes-index))
              (return
               (list (generate-multi-equal-test (List this-node-entry address)'patterns))))))))


;(defun maybe-generate-beta-test (var-entry index not-nodes-index)
;  (loop with addresses
;        for address in (cdr var-entry)
;        for i from 0 to index
;        when address
;        do
;        (push address addresses)   ;Note the addresses are stored in a stack-like fashion
;        finally
;        (return
;	 ;;Only generate a test if the variable appears in at least
;	 ;;2 antecedents
;         (when (cdr addresses)
;	   (List (generate-multi-equal-test
;                  (subseq addresses 0 2)   ;This is an efficiency trick.  Only two addresses
;                  'patterns))))))          ;are needed for the test.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;COMPILE-FC-RULES ---Compiles a list of rules.  NOTE: Only those rules which
;;;require compilation are compiled
(defun compile-fc-rules (rules)
  (dolist (rule rules)
    (unless (rule-compiled? rule)
      (compile-fc-rule rule (if-part rule)(then-part rule)))))


;;;COMPILE-FC-RULE  ---Entry point for the RETE compilation stuff
(defun compile-fc-rule (rule if-part then-part)
  (Let ((*compiling-fc-rule* t))
    (cond (if-part
           (multiple-value-bind (inter-elements-map not-nodes-index)
	       (compile-if-part rule if-part)
             (compile-then-part rule then-part inter-elements-map not-nodes-index)))
          (t
           (compile-then-part rule then-part nil 0)
	   ;;;(add-dummy-instantiation rule)
           ))
    (setf (rule-compiled? rule) t))
    (generate-all-initial-instantiations rule))

(defun compile-if-part (rule if-part)
  (loop with nodes
	with not-nodes
        with more-maps
      	with maps
	for pattern in if-part
        for not-node? = (eq (car pattern) 'not)
        do
        (when not-node?
          (setf pattern (second pattern)))
	(multiple-value-bind (node map)
	    (construct-alpha-node (car pattern) (cdr pattern) not-node?)
          (cond (not-node?
		 (push node not-nodes)
                 (push map more-maps))
                (t
	         (push node nodes)
	         (push map maps))))
        finally
        (return
         (do-beta-nodes rule nodes maps not-nodes more-maps))))

;;;DO-BETA-NODES ---
(defun do-beta-nodes (rule nodes maps not-nodes more-maps)
  (let ((only-dummy? (not nodes))
        not-nodes-index length end-node inter-elements-map)
    (when only-dummy?
      (push (construct-dummy-alpha-node)nodes)
      (setf maps (list nil)))
    (setf not-nodes-index (length nodes))
    (setf nodes (nreverse nodes))
    (set-alpha-nodes-positions nodes)
    (setf nodes (nconc  nodes (nreverse not-nodes))
          maps (nconc (nreverse maps) (nreverse more-maps))
          length (length nodes)
          (beta-node-like? (car nodes)) t
          inter-elements-map (make-inter-elements-map maps)
          end-node (construct-beta-nodes rule nodes
			      inter-elements-map
                                1
                                length
                                (car nodes)
                                not-nodes-index
                                )
          (alpha-nodes rule) nodes)
    end-node ;;;ignore
    (when only-dummy?
      (dummy-input (car nodes)))
    (link-alpha-nodes-to-candidates rule)
    (values inter-elements-map not-nodes-index)))

;;;COMPILE-THEN-PART---This function produces the instantiation form of a forward chaining rule
;;;(i.e. the form which is evaluated to generate an instantiation of a rule).
;;;for instance, the instantiation form of rule foo defined as

;;;(def-rule foo  (foo ?x)(bla ?y) then (tell (bar ?x ?y)))

;;;is

;#.#'(LAMBDA (SUPPORT-SET)
;      (LIST (CONS (QUOTE TELL)
;                  (CONS (CONS (QUOTE BAR)
;                              (CONS (SAFE-ELT (SAFE-ELT SUPPORT-SET 0) 0)
;                                    (CONS (SAFE-ELT (SAFE-ELT SUPPORT-SET 1) 0)
;                                          (QUOTE NIL))))
;                        (QUOTE NIL)))))
;;;When applied to a support set, say '((1)(2)), this form will produce

;;;((TELL (BAR 1 2))).  Computing this form allows efficient  generation of instantiations.
(defun compile-then-part (rule then-part inter-elements-map not-nodes-index)
  (loop for consequent in then-part
        collect (compile-consequent rule consequent inter-elements-map not-nodes-index)
        into consequent-forms
        finally
        (let ((fun (eval
                `(function (lambda (support-set)
                           (List
                           ,@consequent-forms))))))
	(setf (instantiation-form rule)
              #+:lispworks (compile (gensym) fun)
              #-:lispworks (eval fun)))))



;;;COMPILE-CONSEQUENT ---Compiles each consequent in the RHS
(defun compile-consequent (rule consequent inter-elements-map not-nodes-index)
  (let ((*warn-about-free-vars-in-rhs?* (warn-about-free-vars-in-rhs? (car consequent))))
    (transform-rhs-pattern rule consequent inter-elements-map not-nodes-index)))


;;;TRANSFORM-RHS-PATTERN ---This does the real work.  In particular it checks whether we
;;;are inside an ocml-eval form.  If this is the case the safe-elt expressions which substitute
;;;the occurrence of a variable will be quoted.  Otherwise we might get errors caused by 
;;;evaluating the same thing twice (first when instantiating the rule and then when
;;;calling ocml-eval).
(defun transform-rhs-pattern (rule pattern inter-elements-map not-nodes-index
                                   &optional inside-ocml-eval-p)
  (cond ((variable? pattern)
         (generate-var-reference
          rule pattern inter-elements-map not-nodes-index inside-ocml-eval-p))
        ((atom pattern)
         (List 'quote pattern))
        (t
         (unless inside-ocml-eval-p
           (setf inside-ocml-eval-p (eq (car pattern) *ocml-eval-macro-symbol*)))
	 `(cons
             ,(transform-rhs-pattern rule (car pattern) inter-elements-map not-nodes-index
                                     inside-ocml-eval-p)
	     ,(transform-rhs-pattern rule (cdr pattern) inter-elements-map not-nodes-index
                                     inside-ocml-eval-p)))))


;(defun transform-rhs-pattern (rule pattern inter-elements-map not-nodes-index)
;  (cond ((variable? pattern)
;         (generate-var-reference rule pattern inter-elements-map not-nodes-index))
;        ((atom pattern)
;         (List 'quote pattern))
;        (t
;         `(cons
;           ,(transform-rhs-pattern rule (car pattern) inter-elements-map not-nodes-index)
;	   ,(transform-rhs-pattern rule (cdr pattern) inter-elements-map not-nodes-index)))))

(defun generate-var-reference (rule var inter-elements-map not-nodes-index inside-ocml-eval-p)
  (let ((addresses (right-value var inter-elements-map)))
    (cond (addresses
           (let ((address (some #'identity addresses)))
             ;;;((?X (0 0) (1 1) (0 2) (0 3)))
             (if (< (car (last address)) not-nodes-index)
                 (create-address address 'support-set  inside-ocml-eval-p)
                 (if *warn-about-free-vars-in-rhs?*
                     (error "Free variable ~s in the right hand side of rule ~S"
                            var (name rule))
                     (List 'quote var)))))
	  (*warn-about-free-vars-in-rhs?*
            (error "Free variable ~s in the right hand side of rule ~S" var (name rule)))
          (t
           (List 'quote var)))))
    
  

                           
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;The functions below are utilities supporting the generation of test functions

(defun generate-multi-equality-tests (all-vars-addresses
                                      arg
				      &optional (fun #'generate-multi-equal-test))
  (mapcan #'(lambda (addresses)
              (List (funcall fun  addresses arg)))
          all-vars-addresses))

(defun generate-multi-equal-test (addresses arg)
   (cons 'multi-equal
	(mapcar #'(lambda (address)
                    (create-address address arg))
                addresses)))

(defun generate-multi-eq-test (pair arg)
  (cons 'multi-eq
	(cons (list 'quote (car pair))
			  (mapcar #'(lambda (address)
                                      (create-address address arg))
                                  (cdr pair)))))


(defun create-address (address &optional (arg 'args)extra-quote?)
  (let ((form 
  (if (cdr address)
      `(safe-elt ,(create-address (cdr address)arg) ,(car address))
      `(safe-elt ,arg ,(car address)))))
    (if extra-quote?
        `(list 'quote ,form)
        form)))


(defun safe-elt (sequence index)
  (if (and (listp sequence)(< index (length sequence)))
      (elt sequence index)
      (gensym)))

(defun multi-eq (&rest args)
  (if (< (length args) 2)
      t
      (and (eq (car args)(cadr args))
           (apply #'multi-eq (cdr args)))))

(defun multi-equal (&rest args)
  (if (< (length args) 2)
      t
      (and (equal (car args)(cadr args))
           (apply #'multi-equal (cdr args)))))

