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


(defun print-ontology-namespace-declaration (ontology-name ns-label-info stream)
  (format stream "~%  xmlns:~(~a~)=~s"
          (car ns-label-info)
          (or (cdr ns-label-info)
              (default-ontology-namespace-string ontology-name))))

(defun generate-ns-label-alist (ontology)
  (mapcar #'(lambda (onto)
              (cons (name onto)
                    (cons (generate-rdf-namespace-label onto)
                          (ontology-rdf-namespace-url onto))))
          (cons ontology (sub-ontologies ontology))))


(defun generate-rdf-namespace-label (onto)
  (string-downcase (symbol-name  (gentemp (subseq (symbol-name (name onto))
                                                  0
                                                  (min 3 (length (symbol-name (name onto)))))))))
                                                            



(defun generate-rdfs-header (ontology-name ns-label-alist stream )
  (format stream
          "<rdf:RDF~%  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
  xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
  xmlns:dc=\"http://purl.org/dc/elements/1.1#\"")
  (loop for pair in ns-label-alist 
        do
        (print-ontology-namespace-declaration (car pair) (cdr pair) stream))  
  
  (format stream ">")
  
  (format stream 
          "~2%<!-- Automatically translated from OCML ontology ~:(~a~) -->"
          ontology-name))




(defun generate-default-output-file (ontology-name ontology)
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
    

(defun TRANSLATE-OCML-ONTOLOGY-AND-ANCESTORS-TO-RDFS (&optional (ontology *current-ontology*)
                                                                (directory "OCML:LIBRARY;RDFS;"))
  (loop for onto in (cons ontology (sub-ontologies ontology))
        do
        (print (translate-ocml-ontology-to-rdfs (name onto) (concatenate 'string
                                                                  directory
                                                                  (symbol-name (name onto))
                                                                  ".rdfs")))))
                                         
;;;
;;;TRANSLATE-OCML-ONTOLOGY-TO-RDFS - Top level entry point to invoke the ocml->rdfs translator
;;;
(defun translate-ocml-ontology-to-rdfs (&optional (ontology-name (name *current-ontology*)) output-file)
  ;;if no output file a subdirectory called "RDFS" will be created in the directory where
  ;;the ontology has been defined
  (let (
        (ontology (get-ontology ontology-name))
        )
    (cond (ontology
           (let* ((ns-label-alist (generate-ns-label-alist ontology)))
             (unless output-file
               (setf output-file (generate-default-output-file ontology-name ontology)))
             (with-open-file (ofile output-file
                                    :if-does-not-exist :create
                                    :if-exists :supersede
                                    :direction :output)
               (generate-rdfs-header ontology-name ns-label-alist ofile)
               (translate-all-defs-into-rdfs ontology ofile ns-label-alist)
               (format ofile "~%</rdf:RDF>")
               )
             output-file))
          (t
           (error "~s is not a known ontology" ontology-name)))))


(defun translate-all-defs-into-rdfs (ontology ofile ns-label-alist)
  (translate-all-classes ontology ofile  ns-label-alist)
  (translate-all-relations ontology ofile  ns-label-alist)
  (translate-all-instances ontology ofile   ns-label-alist)
  )


(defun translate-all-instances (ontology ofile  ns-label-alist)
  (loop for class in (all-ocml-classes t)
          do
          (loop for instance in (get-current-direct-instances class)
                do
                (when (eq ontology (home-ontology instance))
                  (translate-instance instance  ofile ontology ns-label-alist)))))

(defun translate-instance (instance  ofile ontology ns-label-alist)
  (multiple-value-bind (id label)
                       (convert-generic-resource-name-to-rdf-name-style
                        (name instance)
                        ontology
                        ns-label-alist)
    (let* ((name (name instance))
           (parent (parent-class instance))
           (parent-id (convert-class-or-instance-name-to-rdf-name-style
                       (name parent)
                       (home-ontology parent)
                       ns-label-alist))
           (doc (ocml-documentation instance)))
     ;; (when (eq name 'Event-Of-Launching-Kmi-Planet-Project)
    ;;  (break))
    (format ofile
            "~2%<~a rdf:about=~s>"
            parent-id id
            )
    (when doc
      (format ofile
              "~% <rdfs:comment>~a</rdfs:comment>"doc))
   (format ofile
            "~% <rdfs:label>~a</rdfs:label>"label)

   (loop for slot in (domain-slots parent)
          for values = (setofall '?x `(,slot ,name ?x))
          do
          (loop for value in values
                for slot-id = (convert-relation-name-to-rdf-name-style
                                   slot (home-ontology (get-relation slot)) ns-label-alist)
                do
                (multiple-value-bind (value-id flag)
                                     (format-slot-value value  ontology ns-label-alist)
                  (cond (flag
                         (format ofile
                                 "~% <~a>" slot-id)
                         (format ofile
                                 "~a" value-id)
                         (format ofile
                                 "</~a>" slot-id))
                        (t
                         (format ofile
                                 "~% <~a " slot-id)
                         (format ofile
                                 "~a/>" value-id))))))
                         
                         
      (format ofile
              "~%</~a>"
              parent-id))))

(defun format-slot-value (value  ontology ns-label-alist)
  (cond ((stringp value)
         (if (url? value)
           (format nil "rdf:resource=~s" value)
           (values (remove-bad-rdf-characters value) t)))
        ((numberp value)
         (values (format nil "~a" value) t))
        ((and value (listp value))
         (convert-lisp-list-into-rdf-seq value ontology ns-label-alist))
        (t
         (Let* ((thing (or (find-current-instance value)
                           (get-relation value)))
                (thing-ontology (and thing (home-ontology thing))))
           (if thing
             (format nil "rdf:resource=~s" 
                     (convert-generic-resource-name-to-rdf-name-style
                      (name thing)
                      thing-ontology
                      ns-label-alist))
             (format nil "rdf:resource=~s" 
                     (convert-class-or-instance-name-to-rdf-name-style
                      value
                      ontology
                      ns-label-alist)))))))
        
(defun convert-lisp-list-into-rdf-seq (value ontology ns-label-alist)
  (values (string-append
           "<rdf:Seq>"
           (apply #'string-append
                  (mapcar #'(lambda (el)
                              (multiple-value-bind (value2 flag)
                                                   (format-slot-value el ontology ns-label-alist)
                                (if flag
                                  (string-append "<rdf:li> "
                                                 (format nil "~a"
                                                         value2 )
                                                 " </rdf:li> ")
                                            
                                  (string-append "<rdf:li "
                                                 value2
                                                 " />"))))
                          value))
           "</rdf:Seq>")
          t))


(defun remove-bad-rdf-characters (string)
  (loop with string2 = (replace-char-with-string string #\& " and ")
        for index from 0
        for char across string2
        for newchar = (case char
                        (#\< #\()
                        (#\> #\))
                        (t char))
        do
        (setf (elt string2 index) newchar)
        finally
        (return string2)))
                          

      
(defun translate-all-relations (ontology ofile ns-label-alist)
    (loop for rel in (filter (all-relations t)
                      #'(lambda ( structure)
                          (and (eq (home-ontology structure)
                                    ontology)
                               (= (arity structure) 2))))
          do
          (translate-relation rel  (ocml-documentation rel) ofile ontology ns-label-alist)))

(defun translate-relation (relation  doc ofile ontology ns-label-alist)
  (multiple-value-bind (id label)
                       (convert-relation-name-to-rdf-name-style 
                        (name relation) ontology ns-label-alist)
    (format ofile
            "~2%<rdf:Description rdf:about=~s>"
            id)
    (format ofile
            "~% <rdf:type rdf:resource=~s />"*rdf-Property-id*)
    
    (when doc
      (format ofile
              "~% <rdfs:comment>~a</rdfs:comment>"
              (remove-bad-rdf-characters doc)))
      
    (format ofile
            "~% <rdfs:label>~a</rdfs:label>"
            (remove-bad-rdf-characters label))

    (loop with classes = (local-slot-of relation)
            with all-ranges = nil
            for domain in classes
            do
            (format ofile
                    "~% <rdfs:domain rdf:resource=~s/>"
                    (format nil  "~a" (convert-class-or-instance-name-to-rdf-name-style (name domain)
                                                                            (home-ontology domain)
                                                                            ns-label-alist)))
            (setf all-ranges (append all-ranges (normalize-ranges (get-slot-type domain (name relation)))))
            finally
            (loop  for range in (remove-duplicates all-ranges)
                   do
                  (unless (intersection (mapcar #'name (domain-superclasses (get-ocml-class range)))
                                        all-ranges)
                      (format ofile
                              "~% <rdfs:range rdf:resource=~s/>"
                              (format nil  "~a" (convert-class-or-instance-name-to-rdf-name-style 
                                                     range (home-ontology (get-ocml-class range))
                                                     ns-label-alist))))))
    (format ofile
              "~%</rdf:Description>")))

(defun normalize-ranges (ranges)
  (Let ((or-thing (car (member-if  #'listp ranges))))
     (if or-thing
       (append (cdr or-thing)
               (remove or-thing ranges :test #'equal))
       ranges)))


(defun translate-all-classes (ontology ofile ns-label-alist)
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
          (translate-class class  (documentation class) ofile ontology  ns-label-alist))))

(defun translate-class (class  doc ofile ontology  ns-label-alist)
  (multiple-value-bind (id label)
                       (convert-class-or-instance-name-to-rdf-name-style 
                        (name class) ontology  ns-label-alist)
                        
    (format ofile
            "~2%<rdfs:Class rdf:about=~s>"
            id)
    (when (> (length doc)0)
      (format ofile
              "~% <rdfs:comment>~a</rdfs:comment>"
              (remove-bad-rdf-characters doc)))
    
    (format ofile
            "~% <rdfs:label>~a</rdfs:label>"
            (remove-bad-rdf-characters label))

    (loop for super in (direct-domain-superclasses class)
          do
          
          (format ofile
                  "~% <rdfs:subClassOf rdf:resource=~s/>"
                  (format nil  "~a" (convert-class-or-instance-name-to-rdf-name-style 
                                     (name super)
                                     (home-ontology super)
                                     ns-label-alist))))
          ;;;;(format ofile "~%</rdfs:subClassOf>")
          )
    (format ofile
            "~%</rdfs:Class>"))






(defun convert-generic-resource-name-to-rdf-name-style (name ontology ns-label-alist
                                                             &aux (rel (get-relation name)))
  (if (and rel
           (= (arity rel)2))
    (convert-relation-name-to-rdf-name-style name ontology ns-label-alist)
    (convert-class-or-instance-name-to-rdf-name-style name ontology ns-label-alist)))
    
 
(defun  convert-relation-name-to-rdf-name-style (name ontology ns-label-alist)
  (Let* ((ns (car (right-value (name ontology) ns-label-alist)))
         (string (if (stringp name) name (symbol-name name))) ;;;;(format nil "~a" name)))
         (pretty-string (substitute-if  #\Space 
                                         #'(lambda (char) 
                                             (member char '(#\- #\_))) string)))
    (setf pretty-string (replace-char-with-string pretty-string #\& "And"))
    (Let ((position (position #\space pretty-string)))
      (setf pretty-string
          
                            (if position
                              (string-append (string-downcase (subseq   pretty-string 0 position))
                                             (string-capitalize (subseq   pretty-string  position)))
                              (string-downcase  pretty-string))))
    (values (string-append ns ":" (remove #\Space  pretty-string))
            pretty-string)))


(defun convert-class-or-instance-name-to-rdf-name-style (name ontology ns-label-alist)
  (Let* ((ns (car (right-value (name ontology) ns-label-alist)))
         (string (if (stringp name) name (symbol-name name))) ;;;;(format nil "~a" name)))
         (pretty-string (string-capitalize 
                         (substitute-if  #\Space 
                                         #'(lambda (char) 
                                             (member char '(#\- #\_))) string))))
    (setf pretty-string 
         (replace-char-with-string pretty-string #\& "And"))
    (values (string-append ns ":" (remove #\Space  pretty-string))
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




