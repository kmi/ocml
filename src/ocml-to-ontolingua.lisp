;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")


;;;  STILL TO DO:   <:OWN-SLOTS>, forward chaining rules, 
;;;relation instances (def-relation-instance and tell)

(defvar *list-constructor* 'list-of)

(defvar *skip-form-flag*  :$$skip$$)

(defvar *default-ontolingua-sub-directory* 
   "ONTOLINGUA;")

(defvar *ontolingua-suffix* "onto")

;;;Reminder: This is the syntax accepted by MacLisp
;;;"OCML:library;domains;test-onto;test.lisp"

(defun TRANSLATE-OCML-ONTOLOGY-AND-ANCESTORS-TO-ONTOLINGUA (&optional 
                                                            (ontology *current-ontology*)
                                                           )
  (loop for onto in (cons ontology (sub-ontologies ontology))
        do
        (translate-ocml-ontology-to-ontolingua2  onto)))

(defun translate-ocml-ontology-to-ontolingua (ontology-name)
  (let ((ontology (get-ontology ontology-name)))
    (cond (ontology
           (translate-ocml-ontology-to-ontolingua2 ontology))
          
          (t
           (error "~s is not a known ontology" ontology-name)))))



(defun translate-ocml-ontology-to-ontolingua2 (ontology)
  (let ((ontology-name (name ontology)))
           (translate-ontology-files-to-ontolingua 
            ;;;extra argument added by john domingue
            ;;2/6/03
            (translate-logical-pathname
             (concatenate 'string
                          (default-ontology-pathname 
                           ontology-name
                           (ontology-type ontology))
                          *default-ontolingua-sub-directory*))
            (default-ontology-load-file  
              ontology-name
              (ontology-type ontology))
            (mapcar #'(lambda (file)
                        (merge-pathnames 
                         (ontology-pathname ontology)
                         (make-pathname :name file
                                        :type *lisp-suffix*)))
             (ontology-files ontology)))))



#| changed by john domingue 2/6/03
some incompatibility with logical directories between 
maclisp and lispworks - now pass the directory
(defun translate-ontology-files-to-ontolingua (load-file files)
  (let ((directory (make-pathname :directory 
                                  (concatenate 'string 
                                               (directory-namestring 
                                                load-file)
                                               *default-ontolingua-sub-directory*)
                                  :host (pathname-host load-file))))
    (create-directory directory)
    
    (loop for file in (cons load-file files)
          do
          (translate-ocml-file-to-ontolingua  file directory))))
|#


(defun translate-ontology-files-to-ontolingua (directory load-file files)
  (create-directory directory)
  (loop for file in (cons load-file files)
        do
        (translate-ocml-file-to-ontolingua  
         (translate-logical-pathname file) directory)))



(defun translate-ocml-file-to-ontolingua ( source-pathname target-directory)
  
  ;;(*package* (find-package "OCML"))
  ;;added by johnd domingue 6/2/03 
  ;;to ensure we read into the right package
  (let ((*package* (find-package "OCML"))
        (target-pathname (make-pathname   :host (pathname-host source-pathname)
                                          :directory (pathname-directory target-directory)
                                          :name (Pathname-name source-pathname)
                                          :type *ontolingua-suffix*)))
    (print target-pathname)
    ;;added by johnd 6/2/03
    (create-directory
     (make-pathname :directory (pathname-directory target-directory)))
    ;;added by johnd 6/2/03
    (when (probe-file target-pathname)
      (delete-file target-pathname))
    (with-open-file (ifile source-pathname
                           :direction :input)
      (with-open-file (ofile target-pathname
                             :if-exists :supersede
                             ;;added by john domingue
                             :if-does-not-exist :create
                             :direction :output)
        (format ofile "~%;;;Automatically translated from OCML file ~s"
                source-pathname)
        (loop with new-form
              for form = (read  ifile  nil :eof-value)
              until (eq form :eof-value)
              do
              (setf new-form (translate-ocml-form-into-ontolingua  form))
              (unless (eq new-form *skip-form-flag*)
                (format ofile "~%")
                (princ new-form ofile))
              (format ofile "~%"))))))

(defun translate-ocml-form-into-ontolingua ( form)
  (if (Listp form)
    (cond ((in-package? form)
           (concatenate 'string 
                        "(in-package "
                        (write-to-string "ONTOLINGUA-USER")
                        ")"))
          ((in-ontology? form)
           (concatenate 'string "(in-ontology '" (write-to-string (second form)
                                                                  :case :downcase)
                        ")"))
          ((def-ontology? form)
           (translate-def-ontology-form  form))
          ((def-class-form? form)
           (translate-def-class-form form))
          ((def-axiom? form)
           (translate-def-axiom-form form))
          ((def-instance? form)
           (translate-def-instance-form form))
          ((def-relation? form)
           (translate-def-relation-form form))
          ((def-function? form)
           (translate-def-function-form form))
          ((def-rule? form)
           (destructuring-bind (name &optional documentation &rest clauses)
                               (cdr form)
             (translate-def-rule-form name documentation clauses)))
          (t (Progn 
               (warn "Cannot recognize form ~s" form)
               *skip-form-flag*)))
    (Progn 
      (warn "Cannot recognize form ~s" form)
      *skip-form-flag* )))


(defun translate-def-instance-form (form)
  (destructuring-bind (name super &optional documentation slots)
                      (cdr form)
    (unless (stringp documentation)
      (setf slots documentation
            documentation nil))
    (concatenate 'string
                 (format nil
                         "~%(Define-Individual ~:@(~s ~)(~(~s~))~%"
                         name super)
                 (if documentation
                   (format nil "~s~%" documentation)
                   "")
                 (if slots
                   (concatenate 
                    'string
                    ;;2 spaces added by john domingue
                    ;;6/2/03
                    (format nil  "  :axiom-def~%" )
                    (translate-all-instance-slots-into-axioms name slots))
                   "")
                 ")")))

(defun translate-all-instance-slots-into-axioms (name slots)
  (let ((strings 
         (apply #'append
                (mapcar #'(lambda (slot-spec)
                            (mapcar #'(lambda (slot-value)
                                        ;;2 spaces added by john domingue
                                        ;;6/2/03
                                        (format nil "  (~:(~s~) ~(~s~) ~(~s~))~%"
                                                (car slot-spec) name slot-value))
                                    (cdr slot-spec)))
                        slots))))
    (if (cdr strings)
      (apply #'string-append 
             ;;2 spaces added by john domingue 6/2/03
             (append (cons "  (And " strings)
                     (list "  )")))
      (car strings))))



(defun translate-def-axiom-form (form)
  (destructuring-bind (name &optional documentation body)
                      (cdr form)
    (unless (stringp documentation)
      (setf body documentation
            documentation nil))
    (concatenate 'string
                 (format nil
                         "~%(Define-Axiom ~:@(~s ~)~%"
                         name)
                 (if documentation
                   (format nil "~s~%" documentation)
                   "")
                 (format nil  ":= " )
                 (format nil "~(~s~))"
                         body))))



(defun translate-def-class-form (form)
  (destructuring-bind (def-class name &optional superclasses  instance-var  documentation
                        class-slots &rest relation-spec)
                      form
    def-class ;;ignore
    ;; (print (list name superclasses instance-var documentation))
    (multiple-value-bind (instance-var1  documentation1 class-slots1 relation-spec1)
                         (parse-class-form instance-var  documentation class-slots relation-spec)
      (unless instance-var1 
        (setf instance-var1 (gentemp "?INST")))
      (let (;;;;(supers (translate-superclasses superclasses ))
            (template-slots (translate-class-slots name class-slots1)))
        (concatenate 'string
                     (format nil
                             "~%(Define-Frame ~:@(~s ~)"
                             name)
                     (when (or documentation1 superclasses)
                       ;;2 spaces added by john domingue 6/2/03
                       (format nil "~2%  :own-slots~%"))
                     
                     "  ("
                     (when documentation1
                       ;;2 spaces added by john domingue 6/2/03
                       (format nil "  (Documentation ~s)"
                               documentation1))
                     (when superclasses 
                       ;;2 spaces added by john domingue 6/2/03
                       (format nil "~{~%  (Subclass-of ~(~s~))~}"
                               superclasses))
                     ")"
                     (when template-slots
                       ;;2 spaces added by john domingue 6/2/03
                       (format nil 
                               "~2%  :template-slots~%  (~{~a~%   ~})"
                               template-slots))
                     (destructuring-bind (&key sufficient iff-def constraint slot-renaming &allow-other-keys) relation-spec1 
                       (if (or sufficient iff-def constraint slot-renaming)
                         (concatenate 'string
                                      ;;2 spaces added by john domingue 6/2/03
                                      (format nil 
                                              "~2%  :axioms~%")       
                                      (translate-class-spec-keywords 
                                       sufficient iff-def constraint slot-renaming (list instance-var1) name))
                         ""))
                     ")"
                     )))))

;;;TRANSLATE-DEF-RELATION-FORM
(defun translate-def-relation-form (form)
  (destructuring-bind (name schema &optional documentation &rest relation-spec)
                      (cdr form)
    (unless (stringp documentation)
      (when documentation
        (setf relation-spec (cons documentation relation-spec)
              documentation nil)))
    
    (concatenate 'string
                 (format nil
                         "~%(Define-Relation ~:@(~s~)~( ~s~)~%"
                         name schema)
                 (if documentation
                   (format nil "~s" documentation)
                   "")
                 (destructuring-bind (&key sufficient iff-def constraint &allow-other-keys) relation-spec 
                   (if (or sufficient iff-def constraint)
                     (concatenate 'string
                                  (if sufficient
                                      ;;2 spaces added by john domingue 6/2/03
                                    (format nil "~%  :sufficient ~(~s~)"
                                            sufficient)
                                    "")
                                  (if iff-def
                                      ;;2 spaces added by john domingue 6/2/03
                                    (format nil "~%  :iff-def ~(~s~)"
                                            iff-def)
                                    "")
                                  (if constraint
                                      ;;2 spaces added by john domingue 6/2/03
                                    (format nil "~%  :constraints ~(~s~)"
                                            constraint)
                                    ""))
                     ""))
                 ")"
                 )))

;;;TRANSLATE-DEF-RULE-FORM
(defun translate-def-rule-form (name documentation clauses)
  (cond ((stringp documentation)
         (if (keywordp (car clauses))
           (translate-def-rule-form name documentation (cddr clauses))
           (if (member 'then clauses)
             (Progn 
               (Warn "Cannot translate forward chaining rule ~s.  Ignoring it."
		     name)
               *skip-form-flag*)
             (translate-bc-rule-form name documentation clauses))))
        ((keywordp documentation)
         (translate-def-rule-form name (second clauses) (cddr clauses)))
        (t
         (if (member 'then (cons documentation clauses))
           (Progn 
             (Warn "Cannot translate forward chaining rule ~s.  Ignoring it."
		   name)
             *skip-form-flag*)
           (translate-bc-rule-form name nil (cons documentation clauses))))))

(defun translate-bc-rule-form (name documentation clauses)
  (concatenate 'string
               (format nil
                       "~%(Define-Axiom ~:@(~s ~)~%"
                       name)
               (if documentation
                   ;;2 spaces added by john domingue 6/2/03
                 (format nil "  ~s~%" documentation)
                 "")
               ;;2 spaces added by john domingue 6/2/03
               (format nil  "  := ")
               (translate-bc-rule-clauses   clauses)
               ")"))

(defun translate-bc-rule-clauses (  clauses)
  (cond ((cdr clauses)
         (apply #'string-append
                ;;2 spaces added by john domingue 6/2/03
                (append (List "  (And ")
                        (mapcar #'(lambda (clause)
                                    (translate-bc-rule-clause    clause))
                                clauses)
                        (list ")"))))
        (t
         (translate-bc-rule-clause    (car clauses)))))

(defun translate-bc-rule-clause (clause)
  (destructuring-bind (consequent &optional if &rest antecedents) clause
    (if if
      (concatenate 'string
                   ;;2 spaces added by john domingue 6/2/03
                   (format nil "~%  (=> ") 
                   (if (cdr antecedents)
                     (concatenate 'string
                                  "  (And"
                                  (format nil "~(~{~%   ~s~}~)"
                                          antecedents)
                                  ")")
                     (format nil "~(    ~s~)"
                             (car antecedents)))
                   (format nil "~(~%    ~s~%~)"
                           consequent)
                   ")")
      (format nil "~(~%    ~s~%~)"
              consequent))))




;;;TRANSLATE-DEF-FUNCTION-FORM
(defun translate-def-function-form (form)
  (destructuring-bind (name  schema &optional arrow var documentation &rest options)
                      (cdr form)
    (cond ((eq arrow '->)
           (unless (stringp documentation)
             (when documentation
               (setf options (cons documentation options)
                     documentation nil))))
          
          ((stringp arrow)
           (if var
             (setf options (append (list var documentation)
                                   options)
                   documentation arrow
                   var nil)
             (setf documentation arrow)))
          (arrow
           (setf options
                 (append (list arrow var)
                         (when documentation
                           (list documentation))
                         options)
                 documentation nil
                 var nil)))
    (multiple-value-bind (schema1 options1)
                         (maybe-process-sequence-vars schema options)
      (concatenate 'string
                   (format nil
                           "~%(Define-Function ~:@(~s~)~( ~s~)"
                           name schema1)
                   (if var
                     (format nil " :-> ~(~s~)" var)
                     "")
                   
                   (if documentation
                     (format nil "~%~s" documentation)
                     "")
                   (destructuring-bind (&key def constraint body &allow-other-keys) options1
                     (if (or def constraint body)
                       (concatenate 'string
                                    (if def
                                      (format nil "~%:def ~(~s~)"
                                              def)
                                      "")
                                    (if constraint
                                      (format nil "~%:constraints ~(~s~)"
                                              constraint)
                                      "")
                                    (if body
                                      (format nil "~%:lambda-body ~(~s~)"
                                              body)
                                      "")
                                    )
                       ""))
                   ")"
                   ))))

(defun maybe-process-sequence-vars ( schema options)
  (cond ((member '&rest schema)
         (let* ((oldvar (car (Last schema)))
                (other-vars (butlast (butlast schema)))
                (newvar (read-from-string (substitute #\@ #\? (format nil "~s" oldvar)))))
           (values (append other-vars (list newvar))
                   (subst `(,*list-constructor* ,newvar)
                          oldvar 
                          options))))
        (t
         (values schema options))))


(defun translate-class-spec-keywords (sufficient iff-def constraint slot-renaming schema relation)
  (concatenate 'string
               "("
               (if sufficient 
                 (format nil "~(~S~)~%"
                         `(=> ,sufficient
                              ,(cons relation schema)))
                 "")
               (if iff-def 
                 (format nil "~(~S~)~%"
                         `(<=> ,iff-def
                               ,(cons relation schema)))
                 "")
               (if constraint
                 (format nil "~(~S~)~%"
                         `(=> ,(cons relation schema)
                              ,constraint))
                 "")
               
               (if slot-renaming
                 (translate-slot-renaming slot-renaming schema relation)
                 "")
               ")"))

(defun translate-slot-renaming (slot-renaming schema class)
  (Let ((var (gentemp "?VALUE")))
    (apply #'string-append
           (mapcar #'(lambda (pair)
                       (format nil "~(~S~)~%"
                               `(forall (,(car schema) ,var)
                                        (=> ,(cons class schema)
                                            (<=> (,(car pair) ,(car schema) ,var)
                                                 (,(second pair) ,(car schema) ,var))))))
                   slot-renaming))))




(defun translate-def-ontology-form ( form)
  (let (documentation options)
    (if (stringp (third form))
      (setf documentation (third form)
            options (cdddr form))
      (setf documentation nil
            options (cddr form)))
    ;;pathname added by john domingue 6/2/03
    (destructuring-bind (&key includes type version author allowed-editors files 
                              do-not-include-base-ontology? pathname)
                        options
      allowed-editors pathname;;;ignore
      (let ((super-ontologies (or includes
                                  (unless do-not-include-base-ontology?
                                    (list 'base-ontology))))
            (additional-documentation (if author
                                        (format nil "This is a ~:(~s~) ontology, created by ~:(~A~)."
                                                (or type 'domain) author)
                                        (format nil "This is a ~:(~s~) ontology."
                                                (or type 'domain))))
            
            (even-more-doc (string-append
                            (if files
                             (if (cdr files)
                               (format nil "~%It is factorised in ~s files, ~:@(~{~a, ~}~a~)."
                                       (Length files) (butlast files)(car (last files)))
                               (format nil "~%It is defined in file ~:@(~a~)."
                                       (car files)))
                             "")
                            (if version 
                              (format nil "~%The ontology version number is ~s"
                                                version)
                              ""))))
        (concatenate 'string
                     
                     (format nil
                             "~%(Define-Ontology ~:@(~s~) (~(~{~s ~}~))~%"
                             (second form) includes)
                     (format nil "~%")
                     (format nil
                             "~s"
                             (string-append 
                              (if (stringp documentation)
                                (string-append 
                                 documentation 
                                 (format nil "~%")
                                 additional-documentation)
                                additional-documentation)
                              (or even-more-doc "")))
                     (when super-ontologies 
                       (format nil "~2%:use (~(~{~s ~}~s~))"
                               (butlast super-ontologies) (car (last super-ontologies))))
                     ")"
                     )))))






(defun translate-class-slots (name class-slots)
  (mapcar #'(lambda (spec)
              (translate-each-slot-spec name spec))
          (parse-class-slots name class-slots)))


(defun translate-each-slot-spec ( name spec)
  (destructuring-bind (slot ocml-options clos-options) spec
    clos-options ;;ignore
    (format nil "(~:(~s ~)~{~%   ~a~})"
            slot
            (mapcar #'(lambda (option)
                        (translate-class-slot-option name option))
                    ocml-options))))

(defun translate-class-slot-option (name option)
  (let ((result 
         (case (car option)
           (:type 
            `(slot-value-type ,(maybe-transform-slot-value-type (second option))))
           (:cardinality
            `(slot-cardinality ,(second option)))
           (:min-cardinality 
            `(minimum-slot-cardinality ,(second option)))
           (:max-cardinality 
            `(maximum-slot-cardinality ,(second option)))
           (:value
            `(slot-value ,(second option)))
           (:default-value
             `(default-slot-value ,(second option)))
           (:documentation
            `(documentation ,(second option)))
           (t
            (warn "Can't translate slot option ~s of class ~s"
                  option name)))))
    (when result
      (list (format nil "~(~s~)" (car result))
            (format nil (if (stringp (second result))
                          "~s"
                          "~(~s~)")
                    (second result))))))


(defun maybe-transform-slot-value-type (form)
  (if (symbolp form)
    form
    (if (and (Listp form)
             (eq (car form) 'or))
      (let ((var (gentemp "?VALUE")))
        `'(kappa (,var)
           (or ,@(mapcar #'(lambda (class)
                             `(,class ,var))
                         (cdr form)))))
      (progn
        (warn "~s does not seem to be a correct type restriction..." form)
        form))))

;(Define-Frame Plan
;              :Own-Slots
;              ((Documentation
;                "The Activity-Spec in the Intended-Purpose Relationship")
;               (Instance-Of Class) (Subclass-Of Activity-Spec)
;               (Subclass-Of Qua-Entity))
;             :Template-Slots
;              ((Intended-Purpose (Minimum-Cardinality 0) (Cardinality 1)
;                (Value-Type State-Of-Affairs)))
;              :Axioms
;              (<=> (Plan ?Plan)
;               (Exists (?Soa) (Intended-Purpose ?Plan ?Soa))))


(defun translate-superclasses (superclasses )
  (mapcar #'(lambda (super)
              (list 'Subclass-Of super))
          superclasses))

(defun in-ontology? (form)
  (and 
   (= (length form) 2)
   (eq (car form) 'in-ontology)
   ))

(defun def-ontology? (form)
  (eq (car form) 'def-ontology))


(defun in-package? (form)
  (and 
   (= (length form) 2)
   (eq (car form) 'in-package)))

(defun DEF-CLASS-FORM? (form)
  (eq (car form) 'def-class))

(defun def-axiom? (form)
  (eq (car form) 'def-axiom))

(defun def-instance? (form)
  (eq (car form) 'def-instance))

(defun def-relation? (form)
  (eq (car form) 'def-relation))

(defun def-function? (form)
  (eq (car form) 'def-function))

(defun def-rule? (form)
  (eq (car form) 'def-rule))
