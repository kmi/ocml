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
    

;(defun TRANSLATE-OCML-ONTOLOGY-AND-ANCESTORS-TO-RDFS (&key (fancy-name-conversion?)
;                                                           (ontology *current-ontology*)
;                                                           (directory "OCML:LIBRARY;RDFS;"))
;  (loop for onto in (cons ontology (sub-ontologies ontology))
;        do
;        (print (translate-ocml-ontology-to-rdfs :ontology-name (name onto) 
;                                                :fancy-name-conversion? fancy-name-conversion?
;                                                :output-file (concatenate 'string
;                                                                  directory
 ;                                                                 (symbol-name (name onto))
 ;                                                                 ".rdfs")))))
;


(defun TRANSLATE-OCML-ONTOLOGY-AND-ANCESTORS-TO-RDFS (&key (fancy-name-conversion?)
                                                           (ontology *current-ontology*)
                                                          ;;; (directory "OCML:LIBRARY;RDFS;")
                                                           )
  (loop with ns-label-alist = (generate-ns-label-alist ontology)
        for onto in (cons ontology (sub-ontologies ontology))
        do
        (translate-ocml-ontology-to-rdfs :ontology-name (name onto) 
                                         :fancy-name-conversion? fancy-name-conversion?
                                         :output-file 
                                         (translate-logical-pathname
                                          (concatenate 'string
                                                       (default-ontology-pathname 
                                                         (name onto)
                                                         (ontology-type onto))
                                                       *default-rdfs-sub-directory*
                                                       (symbol-name (name onto))
                                                       ".rdfs"))
                                         :ns-label-alist ns-label-alist)))
                                         
;;;
;;;TRANSLATE-OCML-ONTOLOGY-TO-RDFS - Top level entry point to invoke the ocml->rdfs translator
;;;
(defun translate-ocml-ontology-to-rdfs (&key (fancy-name-conversion?)
                                             (ontology-name (name *current-ontology*))
                                             output-file
                                             ns-label-alist)
  ;;if no output file a subdirectory called "RDFS" will be created in the directory where
  ;;the ontology has been defined
  (let (
        (ontology (get-ontology ontology-name))
        )
    (cond (ontology
           (unless ns-label-alist
             (setf ns-label-alist (generate-ns-label-alist ontology)))
           (unless output-file
             (setf output-file (generate-default-output-file ontology-name ontology)))
           (with-open-file (ofile output-file
                                    :if-does-not-exist :create
                                    :if-exists :supersede
                                    :direction :output)
               (generate-rdfs-header ontology-name ns-label-alist ofile)
               (translate-all-defs-into-rdfs ontology ofile ns-label-alist  fancy-name-conversion? )
               (format ofile "~%</rdf:RDF>")
               )
             output-file)
          (t
           (error "~s is not a known ontology" ontology-name)))))


(defun translate-all-defs-into-rdfs (ontology ofile ns-label-alist  fancy-name-conversion? )
  
  (translate-all-classes ontology ofile  ns-label-alist  fancy-name-conversion? )
  
  (translate-all-relations ontology ofile  ns-label-alist  fancy-name-conversion? )
  
  (translate-all-instances ontology ofile   ns-label-alist  fancy-name-conversion? )
  )


(defun translate-all-instances (ontology ofile  ns-label-alist  fancy-name-conversion? )
  (loop for class in (all-ocml-classes t)
          do
          (loop for instance in (get-current-direct-instances class)
                do
                (when (eq ontology (home-ontology instance))
                  (translate-instance instance  ofile ontology ns-label-alist  fancy-name-conversion? )))))

(defun translate-instance (instance  ofile ontology ns-label-alist  fancy-name-conversion? )
  (multiple-value-bind (id label)
                       (convert-generic-resource-name-to-rdf-name-style
                        (name instance)
                        ontology
                        ns-label-alist  fancy-name-conversion? )
    (let* ((name (name instance))
           (parent (parent-class instance))
           (parent-id (convert-class-or-instance-name-to-rdf-name-style
                       (name parent)
                       (home-ontology parent)
                       ns-label-alist  fancy-name-conversion? ))
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
                                   slot (home-ontology (get-relation slot)) ns-label-alist  fancy-name-conversion? )
                do
                (multiple-value-bind (value-id flag)
                                     (format-slot-value value  ontology ns-label-alist  fancy-name-conversion? )
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

(defun format-slot-value (value  ontology ns-label-alist  fancy-name-conversion? )
  (cond ((stringp value)
         (if (url? value)
           (format nil "rdf:resource=~s" value)
           (values (remove-bad-rdf-characters value) t)))
        ((numberp value)
         (values (format nil "~a" value) t))
        ((and value (listp value))
         (convert-lisp-list-into-rdf-seq value ontology ns-label-alist  fancy-name-conversion? ))
        (t
         (Let* ((thing (or (find-current-instance value)
                           (get-relation value)))
                (thing-ontology (and thing (home-ontology thing))))
           (if thing
             (format nil "rdf:resource=~s" 
                     (convert-generic-resource-name-to-rdf-name-style
                      (name thing)
                      thing-ontology
                      ns-label-alist  fancy-name-conversion? ))
             (format nil "rdf:resource=~s" 
                     (convert-class-or-instance-name-to-rdf-name-style
                      value
                      ontology
                      ns-label-alist  fancy-name-conversion? )))))))
        
(defun convert-lisp-list-into-rdf-seq (value ontology ns-label-alist  fancy-name-conversion? )
  (values (string-append
           "<rdf:Seq>"
           (apply #'string-append
                  (mapcar #'(lambda (el)
                              (multiple-value-bind (value2 flag)
                                                   (format-slot-value el ontology ns-label-alist  fancy-name-conversion? )
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
                          

      
(defun translate-all-relations (ontology ofile ns-label-alist  fancy-name-conversion? )
    (loop for rel in (filter (all-relations t)
                      #'(lambda ( structure)
                          (and (eq (home-ontology structure)
                                    ontology)
                               (= (arity structure) 2))))
          do
          (translate-relation rel  (ocml-documentation rel) ofile ontology ns-label-alist  fancy-name-conversion? )))

;(defun translate-relation (relation  doc ofile ontology ns-label-alist  fancy-name-conversion? )
;  (multiple-value-bind (id label)
;                       (convert-relation-name-to-rdf-name-style 
;                       (name relation) ontology ns-label-alist  fancy-name-conversion? )
;    (format ofile
;            "~2%<rdf:Description rdf:about=~s>"
;           id)
;    (format ofile
;            "~% <rdf:type rdf:resource=~s />"*rdf-Property-id*)
;    
;    (when doc
;      (format ofile
;              "~% <rdfs:comment>~a</rdfs:comment>"
;              (remove-bad-rdf-characters doc)))
;      
;    (format ofile
;            "~% <rdfs:label>~a</rdfs:label>"
;            (remove-bad-rdf-characters label))
;
;    (loop with classes = (local-slot-of relation)
;            with all-ranges = nil
;            for domain in classes
;            do
;            (format ofile
;                    "~% <rdfs:domain rdf:resource=~s/>"
;                    (format nil  "~a" (convert-class-or-instance-name-to-rdf-name-style (name domain)
;                                                                            (home-ontology domain)
;                                                                            ns-label-alist  fancy-name-conversion? )))
;           (setf all-ranges (append all-ranges (normalize-ranges (get-slot-type domain (name relation)))))
;            finally
;           (loop  for range in (remove-duplicates all-ranges)
;                   do
;                  (unless (intersection (mapcar #'name (domain-superclasses (get-ocml-class range)))
;                                       all-ranges)
;                      (format ofile
;                              "~% <rdfs:range rdf:resource=~s/>"
;                              (format nil  "~a" (convert-class-or-instance-name-to-rdf-name-style 
;                                                     range (home-ontology (get-ocml-class range))
;                                                     ns-label-alist  fancy-name-conversion? ))))))
;    (format ofile
;              "~%</rdf:Description>")))
;


;(defun translate-relation (relation  doc ofile ontology ns-label-alist  fancy-name-conversion? )
;  (let ((classes (local-slot-of relation)))
;    (cond ((cdr classes)
;           (setf classes (remove-subsuming-classes ;;;let's remove more specific classes
;                          classes))
;           (cond ((cdr classes) ;;;these do not overlap
;                  (translate-relation-internal relation nil doc ofile ontology 
;                                               ns-label-alist  fancy-name-conversion?)
;                  (loop for class in classes
;                        do
;                        (translate-multi-domain-relation 
;                         relation class doc ofile ontology ns-label-alist  fancy-name-conversion?)))
;                 (t
;                  (translate-relation-internal relation (car classes) doc ofile ontology 
;                                               ns-label-alist  fancy-name-conversion?))))
;          (t
;           (translate-relation-internal relation (car classes) doc ofile ontology 
;                                        ns-label-alist  fancy-name-conversion?)))))

(defun translate-relation (relation  doc ofile ontology ns-label-alist  fancy-name-conversion? )
  (let ((classes (local-slot-of relation)))
    (cond ((cdr classes)
           (translate-relation-internal relation nil doc ofile ontology 
                                               ns-label-alist  fancy-name-conversion?)
           (loop for class in classes
                 do
                 (translate-multi-domain-relation 
                  relation class doc ofile ontology ns-label-alist  fancy-name-conversion?)))
                
          (t
           (translate-relation-internal relation (car classes) doc ofile ontology 
                                        ns-label-alist  fancy-name-conversion?)))))


(defun translate-relation-internal (relation domain doc ofile ontology ns-label-alist  fancy-name-conversion? )
  (multiple-value-bind (id label)
                       (convert-relation-name-to-rdf-name-style 
                        (name relation) ontology ns-label-alist  fancy-name-conversion? )
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
    (when domain

      (format ofile
              "~% <rdfs:domain rdf:resource=~s/>"
              (format nil  "~a" (convert-class-or-instance-name-to-rdf-name-style (name domain)
                                                                                  (home-ontology domain)
                                                                                  ns-label-alist  fancy-name-conversion?)))
      (loop  for range in (get-slot-type domain (name relation))
             do
             (format ofile
                     "~% <rdfs:range rdf:resource=~s/>"
                     (format nil  "~a" (convert-class-or-instance-name-to-rdf-name-style 
                                        range (home-ontology (get-ocml-class range))
                                        ns-label-alist  fancy-name-conversion? )))))
    (format ofile
              "~%</rdf:Description>")))

(defun translate-multi-domain-relation  (relation domain  doc ofile ontology ns-label-alist  fancy-name-conversion? )
  (let ((new-rel-name (gentemp (format nil "~a"(name relation)))))
  (multiple-value-bind (id label)
                       (convert-relation-name-to-rdf-name-style 
                        new-rel-name ontology ns-label-alist  fancy-name-conversion? )
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
    (format ofile "~% <rdfs:subPropertyOf rdf:resource=~s/>"
            (convert-relation-name-to-rdf-name-style 
                       (name relation) ontology ns-label-alist  fancy-name-conversion? ))
            
    (format ofile
              "~% <rdfs:domain rdf:resource=~s/>"
              (format nil  "~a" (convert-class-or-instance-name-to-rdf-name-style (name domain)
                                                                                  (home-ontology domain)
                                                                                  ns-label-alist  fancy-name-conversion?)))
      (loop  for range in (get-slot-type domain (name relation))
             do
             (format ofile
                     "~% <rdfs:range rdf:resource=~s/>"
                     (format nil  "~a" (convert-class-or-instance-name-to-rdf-name-style 
                                        range (home-ontology (get-ocml-class range))
                                        ns-label-alist  fancy-name-conversion? )))))
    (format ofile
              "~%</rdf:Description>")))




(defun normalize-ranges (ranges)
  (Let ((or-thing (car (member-if  #'listp ranges))))
     (if or-thing
       (append (cdr or-thing)
               (remove or-thing ranges :test #'equal))
       ranges)))


(defun translate-all-classes (ontology ofile ns-label-alist  fancy-name-conversion?)
  (format t "XXX hi there!")
  (let (classes)
    (maphash #'(lambda (name structure)
                 name
                 (when (eq (home-ontology structure)
                           ontology)
                   (push structure classes)))
             *domain-classes*)
    (setf classes (sort classes #'(lambda (x y)
                                    (member y (class-precedence-list x)))))
    (loop for class in classes
       do (translate-class class (documentation class 'standard-class)
			   ofile ontology ns-label-alist fancy-name-conversion?))))

(defun translate-class (class  doc ofile ontology  ns-label-alist  fancy-name-conversion? )
 ;; (when (eq class 'INFORMATION-TRANSFER-EVENT)
 ;;   (break "p"))
  (multiple-value-bind (id label)
                       (convert-class-or-instance-name-to-rdf-name-style 
                        (name class) ontology  ns-label-alist  fancy-name-conversion? )
                        
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
                                     ns-label-alist  fancy-name-conversion? ))))
          ;;;;(format ofile "~%</rdfs:subClassOf>")
          )
    (format ofile
            "~%</rdfs:Class>"))


(defun convert-generic-resource-name-to-rdf-name-style (name ontology ns-label-alist  fancy-name-conversion? 
                                                             &aux (rel (get-relation name)))
  (if (and rel
           (= (arity rel)2))
    (convert-relation-name-to-rdf-name-style name ontology ns-label-alist  fancy-name-conversion? )
    (convert-class-or-instance-name-to-rdf-name-style name ontology ns-label-alist  fancy-name-conversion? )))
    
 
(defun  convert-relation-name-to-rdf-name-style (name ontology ns-label-alist  fancy-name-conversion? )
  (Let* ((ns (car (right-value (name ontology) ns-label-alist)))
         (string (if (stringp name) name (symbol-name name))) ;;;;(format nil "~a" name)))
         (pretty-string  (if fancy-name-conversion? 
                           (substitute-if  #\Space 
                                           #'(lambda (char) 
                                               (member char '(#\- 
                                                              #\_))) string)
                           (copy-seq string)
                           )))
    (setf pretty-string (replace-char-with-string pretty-string #\& "And"))
    (Let ((position (position #\space pretty-string)))
      (setf pretty-string
          
                            (if position
                              (string-append (string-downcase (subseq   pretty-string 0 position))
                                             (string-capitalize (subseq   pretty-string  position)))
                              (string-downcase  pretty-string))))
    (values (string-append ns ":" (remove #\Space  pretty-string))
            pretty-string)))


(defun convert-class-or-instance-name-to-rdf-name-style (name ontology ns-label-alist  fancy-name-conversion? )
  (Let* ((ns (car (right-value (name ontology) ns-label-alist)))
         (string (if (stringp name) name (symbol-name name))) ;;;;(format nil "~a" name)))
         (pretty-string   
          (string-capitalize 
           (if fancy-name-conversion? 
             (substitute-if  #\Space 
                             #'(lambda (char) 
                                 (member char '(#\- 
                                                #\_))) string)
              (copy-seq string)
             ))))
    (setf pretty-string 
         (replace-char-with-string pretty-string #\& "And"))
    (values (string-append ns ":" (remove #\Space  pretty-string))
            pretty-string)))


(defun generate-rdf-footer (&optional (stream *standard-output*))
  (format stream "</rdf:RDF>"))




(defun has-documentation-p (string)
  (not (string= string "nil")))




