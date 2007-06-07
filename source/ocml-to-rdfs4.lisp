;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(defvar *rdfs-suffix* "rdfs")

(defvar *default-rdfs-sub-directory* "RDFS;")

(defvar *rdf-namespace* "http://www.w3.org/1999/02/22-rdf-syntax-ns")

(defvar *rdf-Property-id* "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property")

(defun ontology-namespace-declaration (ontology-name ns-label)
  (format nil "xmlns:~(~a~)=\"http://www.aktors.org/ontologies/~(~a~)#\""
          ns-label ontology-name))

(defun default-ontology-namespace-string (ontology-name)
  (format nil "http://www.aktors.org/ontologies/~(~a~)#"
          ontology-name))


(defun generate-default-output-file (ontology-name)
  (Let* ((Load-file (default-ontology-load-file  
                      ontology-name
                      (ontology-type ontology)))
         (directory (make-pathname :directory 
                                   (concatenate 'string 
                                                (directory-namestring load-file)
                                                *default-rdfs-sub-directory*)
                                   :host (pathname-host load-file))))
    
    (make-pathname :host (pathname-host directory)
                   :directory (pathname-directory directory)
                   :name (format nil "~(~a~)" ontology-name)
                   :type *rdfs-suffix*)))
    


(defun translate-ocml-ontology-to-rdfs (ontology-name &optional output-file)
  (let (
        (ontology (get-ontology ontology-name))
        )
    (cond (ontology
           (let* ((ns-label-alist (generate-ns-label-alist ontology-name)))
             (unless output-file
               (setf output-file (generate-default-output-file ontology-name)))
             (with-open-file (ofile output-file
                                    :if-does-not-exist :create
                                    :if-exists :supersede
                                    :direction :output)
               (generate-rdfs-header ontology-name ns-label-alist ofile)
               (translate-all-defs-into-rdfs ontology ofile (default-ontology-namespace-string ontology-name))
               (format ofile "~%</rdf:RDF>"))))
          (t
           (error "~s is not a known ontology" ontology-name)))))


(defun translate-all-defs-into-rdfs (ontology ofile ontology-ns-string)
  (translate-all-classes ontology ofile  ontology-ns-string)
  (translate-all-relations ontology ofile  ontology-ns-string)
  (translate-all-instances ontology ofile   ontology-ns-string)
  )


(defun translate-all-instances (ontology ofile  ontology-ns-string)
  (loop for class in (all-ocml-classes t)
          do
          (loop for instance in (get-direct-instances class)
                do
                (when (eq ontology (home-ontology instance))
                  (translate-instance instance  ofile ontology-ns-string)))))

(defun translate-instance (instance  ofile ontology-ns-string)
  (multiple-value-bind (id label)
                       (convert-resource-name-to-rdf-name-style
                        (name instance))
    (let* ((name (name instance))
           (parent (parent-class instance))
           (parent-id (convert-resource-name-to-rdf-name-style
                       (name parent))))
    (format ofile
            "~2%<~s rdf:about=~s>"
            parent-id
            (string-append  
             ontology-ns-string id))
   (format ofile
            "~% <rdfs:label>~a</rdfs:label>"label)

   (loop for slot in (domain-slots parent)
          for values = (setofall '?x `(,slot ,name ?x))
          do
          (loop for value in values
                do
                (format ofile
                        "~% <~s>" (convert-relation-name-to-rdf-name-style  slot))
                (print-slot-value value ofile)
                (format ofile
                        "~% </~s>" (convert-relation-name-to-rdf-name-style  slot))))
      (format ofile
              "~%</~s>"
              parent-id))))

(defun print-slot-value (value ofile)
  (if (string value)
    (format ofile "~a" value)
    (if (and (get-relation value)
             (= (arity 
                 (get-relation value))
                2))
      (convert-relation-name-to-rdf-name-style  value)
      (convert-resource-name-to-rdf-name-style value))))
      
             

(defun translate-all-relations (ontology ofile  ontology-ns-string)

    (loop for rel in (filter (all-relations t)
                      #'(lambda ( structure)
                          (and (eq (home-ontology structure)
                                    ontology)
                               (= (arity structure) 2))))
          do
          (translate-relation rel  ofile ontology-ns-string)))

(defun translate-relation (relation  ofile ontology-ns-string)
  (multiple-value-bind (id label)
                       (convert-relation-name-to-rdf-name-style 
                        (name relation))
    (format ofile
            "~2%<rdf:Description rdf:about=~s>"
            (string-append  
             ontology-ns-string id))
    (format ofile
            "~% <rdf:type rdf:resource=~s>"*rdf-Property-id*)
    
    
    (format ofile
            "~% <rdfs:label>~a</rdfs:label>"label)
    (let ((classes (local-slot-of relation)))
      
      (loop for domain in classes
            do
            (format ofile
                    "~% <rdfs:domain rdf:resource=~s/>"
                    (string-append  
                     (default-ontology-namespace-string (name (home-ontology domain)))
                     (format nil  "~a" (convert-resource-name-to-rdf-name-style (name domain)))))
            (loop with all-ranges = (get-slot-type domain (name relation))
                  for range in all-ranges
                  do
                  (if (and (listp range) (eq (first range) 'or))
                    (loop for range2 in (cdr range)
                          do
                           (unless (intersection (mapcar #'name (domain-superclasses (get-ocml-class range2)))
                                                 (append (cdr range) all-ranges))
                          
                          (format ofile
                                  "~% <rdfs:range rdf:resource=~s/>"
                                  (string-append  
                                   (default-ontology-namespace-string (name (home-ontology (get-ocml-class range2))))
                                   (format nil  "~a" (convert-resource-name-to-rdf-name-style range2))))))
                    
                    (unless (intersection (mapcar #'name (domain-superclasses (get-ocml-class range)))
                                          all-ranges)
                      (format ofile
                              "~% <rdfs:range rdf:resource=~s/>"
                              (string-append  
                               (default-ontology-namespace-string (name (home-ontology (get-ocml-class range))))
                               (format nil  "~a" (convert-resource-name-to-rdf-name-style range)))))))))
      (format ofile
              "~%</rdf:Description>")))

(defun translate-all-classes (ontology ofile ontology-ns-string)
  (let ((classes)
        )
    (maphash #'(lambda (name structure)
                 name
                 (when (eq (home-ontology structure)
                           ontology)
                   (push structure classes)))
             *domain-classes*)
    (setf classes (sort classes #'(lambda (x y)
                                    (member y (class-precedence-list x)))))
    
    (loop for class in classes
          do
          (translate-class class  ofile ontology-ns-string))))

(defun translate-class (class  ofile ontology-ns-string)
  (multiple-value-bind (id label)
                       (convert-resource-name-to-rdf-name-style 
                        (name class))
    (format ofile
            "~2%<rdfs:Class rdf:about=~s>"
            (string-append  
             ontology-ns-string id))
    
    (format ofile
            "~%<rdfs:label>~a</rdfs:label>"label)
    (loop for super in (direct-domain-superclasses class)
          do
          (format ofile
                  "~%<rdfs:subClassOf rdf:resource=~s/>"
                  (string-append  
                   (default-ontology-namespace-string (name (home-ontology super)))
                   (format nil  "~a" (convert-resource-name-to-rdf-name-style (name super)))))
          ;;;;(format ofile "~%</rdfs:subClassOf>")
          )
    (format ofile
            "~%</rdfs:Class>")
    ))



(defun  convert-relation-name-to-rdf-name-style (name)
  (Let* ((string (if (stringp name) name (symbol-name name))) ;;;;(format nil "~a" name)))
         (pretty-string (substitute-if  #\Space 
                                         #'(lambda (char) 
                                             (member char '(#\- #\_))) string)))
    (setf pretty-string (replace-char-with-string pretty-string #\& "And"))
    (Let ((position (position #\space pretty-string)))
      (if position
        (setf pretty-string (string-append (string-downcase (subseq   pretty-string 0 position))
                       (string-capitalize (subseq   pretty-string  position))))
         (setf pretty-string (string-downcase  pretty-string))))
    (values (remove #\Space  pretty-string)
            pretty-string)))


(defun convert-resource-name-to-rdf-name-style (name)
  (Let* ((string (if (stringp name) name (symbol-name name))) ;;;;(format nil "~a" name)))
         (pretty-string (string-capitalize 
                         (substitute-if  #\Space 
                                         #'(lambda (char) 
                                             (member char '(#\- #\_))) string))))
    (setf pretty-string (replace-char-with-string pretty-string #\& "And"))
    (values (remove #\Space  pretty-string)
            pretty-string)))



(defun translate-ocml-file-to-rdfs (source-file ofile)
  
  (with-open-file (ifile source-file
                         :direction :input)
    (loop with new-form
          for form = (read  ifile  nil :eof-value)
          until (eq form :eof-value)
          do
          (setf new-form (translate-ocml-form-into-rdfs  form ))
          (unless (eq new-form *skip-form-flag*)
            (format ofile "~%")
            (princ new-form ofile))
          (format ofile "~%"))))

(defun translate-ocml-form-into-rdfs  (form)
  form)


(defun generate-rdfs-header (ontology-name ns-label stream )
  (format stream
          "<rdf:RDF~%  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
  xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
  xmlns:dc=\"http://purl.org/dc/elements/1.1/\"")
  
  
  
  (format stream
          "~%  ~a>" (ontology-namespace-declaration ontology-name ns-label))
  (format stream 
          "~2%<rdf:Description>
  <dc:creator>Automatically translated from OCML ontology ~:(~a~) </dc:creator>"
          ontology-name)
  (format stream 
          "~%  <dc:type>ontology</dc:type>~%</rdf:Description>~%"))



(defun translate-ocml-form-into-rdfs (form )
  (if (Listp form)
    (cond ((in-package? form)
           "")
          ((in-ontology? form)
           "")
          ((def-ontology? form)
           "")
          ((def-class-form? form)
           (translate-def-class-form-to-rdfs form))
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
               (warn "Cannot recognize form ~s"
                     form)
               *skip-form-flag* )))
    (Progn 
      (warn "Cannot recognize form ~s"
            form)
      *skip-form-flag* )))

(defun translate-def-class-form-to-rdfs (form)
  (destructuring-bind (def-class name &optional superclasses  instance-var  documentation
                        class-slots &rest relation-spec)
                      form
    def-class ;;ignore
    (multiple-value-bind (instance-var1  documentation1 class-slots1 relation-spec1)
                         (parse-class-form instance-var  documentation class-slots relation-spec)
      (unless instance-var1 
        (setf instance-var1 (gentemp "?INST")))
      (let (
            (template-slots (translate-class-slots name class-slots1)))
        (concatenate 'string
                     (format nil
                             "~%(Define-Frame ~:@(~s ~)"
                             name)
                     (when (or documentation1 superclasses)
                       (format nil "~2%:own-slots~%"))
                     
                     "("
                     (when documentation1
                       (format nil "(Documentation ~s)"
                               documentation1))
                     (when superclasses 
                       (format nil "~{~%(Subclass-of ~(~s~))~}"
                               superclasses))
                     ")"
                     (when template-slots 
                       (format nil 
                               "~2%:template-slots~%(~{~a~%~})"
                               template-slots))
                     (destructuring-bind (&key sufficient iff-def constraint slot-renaming &allow-other-keys) relation-spec1 
                       (if (or sufficient iff-def constraint slot-renaming)
                         (concatenate 'string
                                      (format nil 
                                              "~2%:axioms~%")       
                                      (translate-class-spec-keywords 
                                       sufficient iff-def constraint slot-renaming (list instance-var1) name))
                         ""))
                     ")"
                     )))))




(defun generate-rdf-footer (&optional (stream *standard-output*))
  (format stream "</rdf:RDF>"))




(defun has-documentation-p (string)
  (not (string= string "nil")))




