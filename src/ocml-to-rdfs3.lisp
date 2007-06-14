;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(defvar *rdfs-suffix* "rdfs")

(defvar *default-rdfs-sub-directory* "RDFS;")

(defun ontology-namespace-declaration (ontology-name ns-label)
  (format nil "xmlns:~(~a~)=\"http://www.aktors.org/ontologies/~(~a~)#\""
          ns-label ontology-name))

(defun ontology-namespace-string (ontology-name)
  (format nil "http://www.aktors.org/ontologies/~(~a~)#"
          ontology-name))





(defun translate-ocml-ontology-to-rdfs (ontology-name ns-label)
  (let ((ontology (get-ontology ontology-name))
        )
    (cond (ontology
           (let* ((Load-file (default-ontology-load-file  
                               ontology-name
                               (ontology-type ontology)))
                  (directory (make-pathname :directory (concatenate 'string 
                                                                    (directory-namestring load-file)
                                                                    *default-rdfs-sub-directory*)
                                            :host (pathname-host load-file)))
                  (target-pathname (make-pathname :host (pathname-host directory)
                                                  :directory (pathname-directory directory)
                                                  :name (format nil "~(~a~)" ontology-name)
                                                  :type *rdfs-suffix*)))
             (create-directory directory)
             (with-open-file (ofile target-pathname
                                    :if-does-not-exist :create
                                    :if-exists :supersede
                                    :direction :output)
               (generate-rdfs-header ontology-name ns-label ofile)
               (translate-all-defs-into-rdfs ontology ofile (ontology-namespace-string ontology-name))
               (format ofile "~%</rdf:RDF>"))))
          (t
           (error "~s is not a known ontology" ontology-name)))))


(defun translate-all-defs-into-rdfs (ontology ofile ontology-ns-string)
  (translate-all-classes ontology ofile  ontology-ns-string)
  ;;(translate-alll-relations ontology ofile  ontology-ns-string)
  ;;(translate-all-instances ontology ofile   ontology-ns-string)
  )


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
                       (convert-class-name-to-rdf-name-style 
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
                   (ontology-namespace-string (name (home-ontology super)))
                   (format nil  "~a" (convert-class-name-to-rdf-name-style(name super)))))
          ;;;;(format ofile "~%</rdfs:subClassOf>")
          )
    (format ofile
            "~%</rdfs:Class>")
    ))



(defun convert-class-name-to-rdf-name-style (name)
  (Let* ((string (if (stringp name) name (format nil "~a" name)))
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




