;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

 

;;;PARSE-ASSERTION  --- This function is used to parse an assertion (i.e. a relation instance).
;;;Parse-assertion only checks that the assertion is a list, creates the associated predicate
;;;if it does not already exist, and evaluates all functional terms in the assertion.
;;;It returns (VALUES Predicate args original-form)
(defun parse-assertion (exp &optional env)
  (if (listp exp)
      (destructuring-bind (pred &rest args) exp
        (when (variable? pred)
          (multiple-value-bind (flag pred2)
              (lookup pred env)
            (cond ((or (not flag) (variable? pred2))
                   (error "A variable is not a legal relation name..when asserting ~S in ~S"
                          (cons pred args) env))
                  ((consp pred2)
                   (error "A list is not a legal relation name..when asserting ~S in ~S"
                          (cons pred2 args)))
                  (t
                   (setf pred pred2)))))
	(values
	 (find-or-create-relation pred (length args))
	 (eval-or-instantiate-args args env)
	 exp))
      (error "An assertion must be a list...when parsing ~s" exp)))

;(defun parse-assertion (exp &optional env)
;  (if (listp exp)
;      (destructuring-bind (pred &rest args) exp
;        (when (variable? pred)
;          (setf pred (instantiate pred env)))
;        (values
;         (find-or-create-relation pred (length args))
;	 (eval-or-instantiate-args args env)
;         exp))
;      (error "An assertion must be a list...when parsing ~s" exp)))

(defun parse-assertion-to-delete (exp &optional env &aux pred)
  (cond ((listp exp)
         (if (variable? (car exp))
             (setf pred (lookup (car exp) env))
             (setf pred (car exp)))
      (let ((pred-structure (get-relation pred)))
        (and pred-structure
             (values pred-structure
                     (eval-or-instantiate-args (cdr exp)env )
                     ))))
      (t
       (error "An assertion must be a list...when parsing ~s" exp))))


(defun parse-define-relation-form (name schema documentation options
                                        &optional (type 'relation))
  (check-name name)
  (cond ((and (listp schema) (stringp documentation))
         (values name schema  documentation options))
        ((and (listp schema)
              (keywordp documentation))
         (values name schema  nil (cons documentation options)))
        ((and (listp schema)
              (null documentation)
              (null options))
         (values name schema  nil nil))
        ((listp schema)
         (error "Bad syntax for DEFINE-~S form --when parsing ~S" type
                     `(,type ,name ,schema ,documentation ,@options)))
        ((and (stringp schema)
              (keywordp documentation))
         (values name nil  schema (cons documentation options)))
        ((stringp schema)
         (error "Bad syntax for DEFINE-~S form --when parsing ~S" type
                     `(,type ,name ,schema ,documentation ,@options)))
        ((keywordp schema)
         (values name nil  nil (cons schema (cons documentation options))))
        (t
         (error "Bad syntax for DEFINE-~S form --when parsing ~S" type
                     `(,type ,name ,schema ,documentation ,@options)))))

(defun check-name (name)
  (or (symbolp name)
      (error "Bad name. Only symbols can be used as names.....when parsing ~S"
             name)))


(defun parse-define-function-form (name schema documentation options)
  (parse-define-relation-form name schema documentation options 'function))

;(defun parse-query (query)
;  (if (listp query)
;      (let ((pred (car query)))
;        (cond ((member pred '(and or))
;               (cons pred (mapcar #'parse-query (cdr query))))
;              ((eq (car query)'not)
;               (list 'not (parse-query (second query))))
;              ((eq (car query) 'exists)
;               (parse-query (third query)))
;              ((eq (car query) 'forall)
;               (list 'forall (second query)
;                     (parse-query (third query))
;                     (parse-query (fourth query))))
;              (t
;               (parse-simple-query pred (cdr query)))))
;      (error "A query must be a list ...when  parsing ~s" query)))

;(defun parse-simple-query (pred args)
;  (cons pred (evaluate-fun-terms args)))
      

;;;*********************************************************************
;;;PARSE-CLASS-FORM --Standardizes the argument list
(defun parse-class-form (instance-var documentation class-slots relation-spec)
  (cond ((keywordp instance-var)
         (values nil nil nil 
                 (nconc (list instance-var documentation)
                        (when (keywordp class-slots)
                          (cons class-slots relation-spec)))))
	((keywordp documentation)
         (cond ((variable? instance-var)
                (values instance-var 
                        nil nil 
                 (nconc (list documentation class-slots) relation-spec)))
               ((and (listp instance-var)
                    (= (length instance-var) 1)
                    (variable? (car instance-var)))
                (values (car instance-var)
                        nil nil 
                        (nconc (list documentation class-slots) relation-spec)))
                
	       ((stringp instance-var)
                (values nil instance-var nil
                        (nconc (list documentation class-slots)relation-spec)))
	       (t
                ;;instance-var = list of slots, or NIL
                (values nil nil   
                        instance-var
                        (nconc (list documentation class-slots)relation-spec)))))
        ((stringp instance-var)
         ;;documentation must be a list of slots, or NIL
         (values nil instance-var documentation
                 (when (keywordp class-slots)
                   (cons class-slots relation-spec))))
        ((stringp documentation)
         (cond ((variable? instance-var)
                (if (keywordp class-slots)
	          (values instance-var documentation nil (cons class-slots relation-spec))
                  (values instance-var documentation class-slots relation-spec)))
               ((and (listp instance-var)
                     (= (length instance-var) 1)
                     (variable? (car instance-var)))
                (if (keywordp class-slots)
	          (values (car instance-var) documentation nil (cons class-slots relation-spec))
                  (values (car instance-var) documentation class-slots relation-spec)))
               (t
                (error 
                 "Can't parse DEF-CLASS form, ~S - ~s should be a variable or a schema with arity 1"
                       `(DEF-CLASS instance-var  documentation class-slots ,@relation-spec)
                       instance-var))))
        ((variable? instance-var)
         (values instance-var nil documentation
                 (when (keywordp class-slots)
                   (cons class-slots relation-spec))))
        ((and (listp instance-var)
              (= (length instance-var) 1)
              (variable? (car instance-var)))
         (values (car instance-var) nil documentation
                 (when (keywordp class-slots)
                   (cons class-slots relation-spec)))
                )
        ((listp instance-var)
         (values nil nil  instance-var nil))
        (t
	 (error "Can't parse DEF-CLASS form, ~S"
                `(DEF-CLASS instance-var  documentation class-slots ,@relation-spec)))))


;(defun parse-class-form (instance-var slot-value-var documentation class-slots relation-spec)
;  (cond ((keywordp instance-var)
;         (values nil nil nil nil
;                 (nconc (list instance-var slot-value-var)
;                        (when (keywordp documentation)
;			  (List documentation class-slots))
;                         relation-spec)))
;        ((keywordp slot-value-var)
;         (cond ((variable? instance-var)
;                (values instance-var nil nil nil
;                 (nconc (list slot-value-var documentation)
;                        (when (keywordp class-slots)
;			  (cons class-slots relation-spec)))))
;               ((stringp instance-var)
;                (values nil nil  instance-var nil
;                        (nconc (list slot-value-var documentation)
;                        (when (keywordp class-slots)
;			  (cons class-slots relation-spec)))))
;               (t
;                (values nil nil  nil  instance-var
;                        (nconc (list slot-value-var documentation)
;                        (when (keywordp class-slots)
;			  (cons class-slots relation-spec)))))))
;        ((variable? slot-value-var)
;         (cond ((stringp documentation)
;                (if (keywordp class-slots)
;                    (values instance-var slot-value-var documentation nil
;                            (cons class-slots relation-spec))
;                    (values instance-var slot-value-var documentation class-slots relation-spec)))
;               ((keywordp  documentation)
;                (values instance-var slot-value-var nil nil
;                        (nconc (list documentation  class-slots )
;                               relation-spec)))
;	       (t
;                (values  instance-var slot-value-var nil documentation
;                         (when (keywordp class-slots)
;			  (cons class-slots relation-spec))))))
;        ((variable? instance-var)
;         (cond ((stringp slot-value-var)
;                (if (keywordp documentation)
;                    (values  instance-var nil slot-value-var  nil
;                             (nconc (list documentation class-slots)
;                                    relation-spec))
;                    (values  instance-var nil slot-value-var  documentation
;                             (when (keywordp class-slots)
;                               (cons class-slots relation-spec)))))
;               (t
;                (values  instance-var nil nil slot-value-var
;                         (when (keywordp documentation)
;                           (nconc (List documentation class-slots)
;                                  relation-spec))))))
;        ((stringp instance-var)
;	 (if (keywordp slot-value-var)
;             (values  nil nil instance-var nil
;                      (nconc (list slot-value-var documentation)
;                             (when (keywordp class-slots)
;                               (cons class-slots relation-spec))))
;             (values nil nil instance-var  slot-value-var
;                     (when (keywordp documentation)
;                       (nconc (list documentation class-slots)
;                              relation-spec)))))
;        ((listp instance-var)                              ;;It could be NIL!
;         ;; instance-var = class slots
;         (if (keywordp slot-value-var)
;             (values  nil nil nil instance-var
;                      (nconc (list slot-value-var documentation)
;                             (when (keywordp class-slots)
;                               (cons class-slots relation-spec))))
;             (values  nil nil nil instance-var nil)))
;        (t
;         (error "Can't parse DEF-CLASS form, ~S"
;                `(DEF-CLASS instance-var slot-value-var documentation class-slots ,@relation-spec)))))
          

;;;PARSE-CLASS-SLOTS ---Splits slot specifications into ocml and clos parts
(defun parse-class-slots (name class-slots)
  (loop 
        with domain-slots = nil
	for slot-spec in class-slots
        do
        (multiple-value-bind (ocml-options clos-options)
            (parse-class-slot-spec2 name slot-spec)
	  (push (list (car slot-spec)
                          ocml-options
			  clos-options)
                    domain-slots))
        finally
        (return  domain-slots)))


;(defun parse-class-slots (name class-slots)
;  (loop with superclasses = nil
;        with domain-slots = nil
;       
;        for slot-spec in class-slots
;        do
;        (multiple-value-bind (flag ocml-options-or-superclasses clos-options)
;            (parse-class-slot-spec2 name slot-spec)
;          (if (eq flag :subclass-of-slot)
;              (setf superclasses (append superclasses ocml-options-or-superclasses ))
;	      (push (list (car slot-spec)
;                          ocml-options-or-superclasses
;			  clos-options)
;                    domain-slots)))
;        finally
;        (return
;         (values superclasses domain-slots))))


(defun legal-slot-option? (option)
  (or (member option *legal-ocml-slot-options*)
      (member option *legal-clos-slot-options*)))

(defun ocml-option? (option)
  (member option *legal-ocml-slot-options*))

(defun clos-option? (option)
  (member option *legal-clos-slot-options*))

(defun value-option? (option)
  (member option *value-options*))

;(defun parse-class-slot-spec2 (name slot-spec)
;  (let ((l (Length slot-spec)))
;    (if (oddp l)
;      (loop 
;	with clos-options
;	with ocml-options
;	for i from 1 to (- (length slot-spec) 2) by 2
;	for option = (elt slot-spec i)
;	for value = (elt slot-spec (1+ i))
;	do
;	(cond ((ocml-option? option)
;               (when (member option '(:value :default-value))
;                 (setf value (maybe-evaluate-term value)))
;	       (push (list option value) ocml-options))
;	      ((clos-option? option)
;	       (push (list option value) clos-options))
;	      (t
;	       (error "~S is not a legal slot option...when parsing ~S of ~S"
;		      option slot-spec name)))
;	finally
;	(return
;	 (values  ocml-options clos-options)))
;      (error "~S is not a legal slot specification..when parsing class ~S"
;		     slot-spec name))))

;;;new from enrico 27/11/98
(defun parse-class-slot-spec2 (name slot-spec)
  (let ((l (Length slot-spec)))
    (cond ((oddp l)
           (loop
             with clos-options
             with ocml-options
             for i from 1 to (- (length slot-spec) 2) by 2
             for option = (elt slot-spec i)
             for value = (elt slot-spec (1+ i))
             do
             (cond ((ocml-option? option)
                    (when (member option '(:value :default-value))
                      (setf value (maybe-evaluate-term value)))
                    (push (list option value) ocml-options))
                   ((clos-option? option)
                    (push (list option value) clos-options))
                   (t
                    (error "~S is not a legal slot option...when parsing ~S
of ~S"
                           option slot-spec name)))
             finally
             (return
              (values  ocml-options clos-options))))
          ((= l 2)
           ;;we assume that a :value option is implicit
           (values (List (list :value (maybe-evaluate-term (second slot-spec))))
                   nil))

          (t
           (error "~S is not a legal slot specification..when parsing class ~S"
                  slot-spec name)))))

;(defun parse-class-slot-spec2 (name slot-spec)
;  (let ((slot (car slot-spec))
;        (clos-options)
;        (ocml-options))
;    (cond ((eq slot *subclass-of-slot*)
;           (values :subclass-of-slot
;                   (cdr slot-spec)))
;	  (t
;           (loop for i from 1 to (- (length slot-spec) 2) by 2
;                 for option = (elt slot-spec i)
;                 for value = (elt slot-spec (1+ i))
;                 do
;                 (cond ((ocml-option? option)
;                        (push (list option value) ocml-options))
;                       ((clos-option? option)
;                        (push (list option value) clos-options))
;                       (t
;			(error "~S is not a legal slot option...when parsing ~S of ~S"
;                               option slot-spec name)))
;                 finally
;                 (return
;                  (values :domain-slot ocml-options clos-options)))))))


;(defun parse-class-slot-spec (name slot-spec)
;  (let ((slot (car slot-spec))
;        (other-options)
;        (value)
;        (value-keyword)
;        )
;    (cond ((eq slot *subclass-of-slot*)
;           (values (cdr slot-spec)
;                   :superclasses))
;	  (t
;           (loop for i from 1 to (- (length slot-spec) 2) by 2
;                 for option = (elt slot-spec i)
;                 do
;                 (if (legal-slot-option? option)
;                     (if (value-option? option)
;                         (progn
;                           (setf value-keyword option)
;                           (setf value (elt slot-spec (1+ 1))))
;                         (push (cons option (elt slot-spec (1+ 1)))
;                               other-options))
;                     (error "~S is not a legal slot option...when parsing ~S of ~S"
;                            option slot-spec name))
;                 finally
;                 (return
;                  (values value value-keyword  other-options)))))))
                       
                 
          
;(defun parse-instance-slots (name slots)
;  (loop with parent
;        with domain-slots
;        for slot-spec in slots
;        do
;        (destructuring-bind (slot &rest values) slot-spec
;          (if (eq slot *instance-of-slot*)
;            (if (cdr values)
;                (error "Only one parent allowed for an instance..when parsing ~S"
;                       name)
;                (setf parent (car values)))
;            (setf domain-slots (append (list slot values)domain-slots))))
;        finally
;        (return (values parent domain-slots))))