;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defun get-ontology (name)
  (right-value name  *all-ontologies*))

(defun get-or-create-ontology (name &optional includes)
  (or (get-ontology name)
      (def-ontology-internal name nil includes)))

(defun add-ontology (name object)
  (push (cons name object)*all-ontologies*))

(defun replace-ontology (name object)
  (setf (right-value name *all-ontologies*)
        object))

(defun clean-up-deleted-ontology-links (ontology)
  (mapc #'(lambda (o)
	    (setf (ontology-includes o)
		  (remove ontology (ontology-includes o))))
	(ontology-included-by ontology))
  (mapc #'(lambda (o)
	    (setf (ontology-included-by o)
		  (remove ontology (ontology-included-by o))))
	(ontology-includes ontology)))

(defun remove-ontology-internal (name ontology)
  (clean-up-deleted-ontology-links ontology)
  (setf *all-ontologies* (remove (cons name ontology) *all-ontologies* :test #'equal)))

;(eval-when (eval load)
;  (unless *rule-packets*
 ;   (initialize-rule-packets)))

(defstruct (ontology-directory (:conc-name "ONTOLOGY-"))
  (relations (make-hash-table))
  ;;(operators (make-hash-table))
  (default-fc-rule-packet nil)
  (other-fc-rule-packets nil)
  (axioms (make-hash-table))
  ;;;;(fc-rule-packets nil)
  (bc-rules (make-hash-table))
  (functions (make-hash-table))
  ;;;;(tasks (make-hash-table))
  ;;;;(roles (make-hash-table))
  (classes (make-hash-table)))
        
;;;; OCML-ONTOLOGY

;;; WebOnto wants other slots in the ocml-ontology class, so allow it
;;; to safely redefine the class OCML-ONTOLOGY by putting the good
;;; bits in ALMOST-OCML-ONTOLOGY, and making OCML-ONTOLOGY a facade
;;; that backs straight onto ALMOST-OCML-ONTOLOGY
(defclass almost-ocml-ontology (name-mixin documentation-mixin)
  ((includes :initarg :includes :accessor ontology-includes)
   (included-by  :initform nil :accessor ontology-included-by)
   (directory :accessor ontology-directory :initform (make-ontology-directory))
   ;;the default is domain because end-users will usually build
   ;;domain ontologies. Other values are :basic :method, :task and :application
   (ontology-type :accessor ontology-type :initarg :type :initform :domain)
   (author :accessor ontology-author :initarg :author :initform "kmi")
   (allowed-editors :initarg :allowed-editors :accessor ontology-allowed-editors 
                    :initform nil)
   (pathname :accessor ontology-pathname :initarg :pathname :initform nil)
   (ontology-files :accessor ontology-files :initarg :files :initform nil)
   (version :accessor ontology-version-number :initarg :version :initform 1.0)
   ;; The namespace URI is used with the #_ reader macro to place
   ;; symbols in a particular namespace.  It need not point at
   ;; anything real, or even look like a URI :-) It should be unique.
   (namespace-uri :accessor namespace-uri-of :type string
		  :initarg :namespace-uri :initform nil)
   ;;(rdf-namespace-label :accessor ontology-rdf-namespace-label
   ;;                     :initform nil
   ;;                     :initarg :rdf-namespace-label)
   (rdf-namespace-url :accessor ontology-rdf-namespace-url
                      :initform nil
                      :initarg :rdf-namespace-url)))

(defclass ocml-ontology (almost-ocml-ontology)
  ())

(defun ontology? (thing)
  (typep thing 'ocml-ontology))

(defun known-ontology? (struct)
  (and (left-value struct  *all-ontologies*)
       struct))
  

(defun sub-ontologies (ontology)
  (let ((direct-sub-ontologies (ontology-includes ontology)))
    (remove-duplicates 
     (append direct-sub-ontologies 
             (mapcan* #'sub-ontologies
                      direct-sub-ontologies)))))

(defun remove-subsumed-ontologies (ontologies)
  (remove-if   #'(lambda (x)
                   (some #'(lambda (y)
                             (member x (sub-ontologies y)))
                         (remove x ontologies)))
               ontologies))
        

(defun dependent-ontologies (ontology)
  (let ((direct-dependents (ontology-included-by ontology)))
    (remove-duplicates 
     (append direct-dependents
             (mapcan* #'dependent-ontologies
                      direct-dependents)))))

(defun sorted-dependent-ontologies (ontology)
  "Sorts the dependent ontoogies from the most generic
   to the most specific"
  (sort  (dependent-ontologies ontology)
         #'(lambda (x y)
             (member x (sub-ontologies y)))))
        

;;;HOME-ONTOLOGY-OF
;;(defun home-ontology-of (name type)
;;  (case type
;;    (class (home-ontology (get-ocml-class name)))
;;    (instance (home-ontology (find-instance name)))
;;    (relation (home-ontology (get-relation name)))
;;    (function (home-ontology (get-function name)))))

;;;PRINT-OBJECT
(defmethod print-object ((ontology ocml-ontology) stream)
  (with-slots (name) ontology
    (format stream "#<~S ~S>" 'OCML-ONTOLOGY name)))


(defun make-default-rule-packet (ontology)
  (make-instance 'rule-packet 
   :name (read-from-string    ;;:default-rule-packet
          (string-append 
           ":DEFAULT-RULE-PACKET-IN-"
           (string (name ontology))))))

(defun ontology-rule-packets (&optional (onto *current-ontology*))
  (let ((dir (ontology-directory onto)))
    (cons (ontology-default-fc-rule-packet dir)
          (ontology-other-fc-rule-packets dir))))

(defun find-rule-packet-in-ontology (name &optional (onto *current-ontology*))
  (let ((default (ontology-default-fc-rule-packet
                  (ontology-directory onto))))
    (if (eq name (name default))
      default
     (find name (ontology-other-fc-rule-packets
                               (ontology-directory onto))
                         :test  #'(lambda (x y)
                                    (declare (ignore x))
                                    (eq (name y)name))))))

(defun remove-rule-packet-from-ontology (name &optional (onto *current-ontology*))
  (let ((default (ontology-default-fc-rule-packet
                  (ontology-directory onto))))
    (if (eq name (name default))
      (warn "can't remove the default rule packet from ontology ~s"
            (name onto))
      (setf (ontology-other-fc-rule-packets
                    (ontology-directory onto))
            (remove-if #'(lambda (x)
                           (eq (name x) name))
                       (ontology-other-fc-rule-packets
                        (ontology-directory onto)))))))


(defun add-rule-packet-in-ontology (packet &optional (onto *current-ontology*))
  (push packet (ontology-other-fc-rule-packets
                (ontology-directory onto)))
  (propagate-new-rule-packet-to-sub-ontologies   packet))

(defun clear-all-packets (&optional (onto *current-ontology*))
  (let ((dir (ontology-directory onto)))
  (setf (ontology-default-fc-rule-packet dir )
        (make-default-rule-packet onto)
        (ontology-other-fc-rule-packets dir ) nil)))

(defun default-fc-rule-packet (&optional (onto *current-ontology*))
  (ontology-default-fc-rule-packet (ontology-directory onto)))

(defun def-ontology-internal (name documentation options)
  (unless (stringp documentation)
    (setf options (when options
                    (cons documentation options))
          documentation nil))
  (apply #'def-ontology-internal2 (append (list name documentation)options)))

#|
changed by john domingue 6/2/03
(defun default-ontology-pathname (name type)
  (string-append *library-pathname*
                 (format nil "~AS;~A;" type name)))
|#

(defun default-ontology-pathname (name type)
  (if (eq type :basic)
      (string-append *library-pathname*
                     (format nil "~A;" type))
    (string-append *library-pathname*
                   (format nil "~AS;~A;" type name))))

(defun default-ontology-files (name )
  (list (format nil "~A" name)
        "new"))

(defun default-ontology-load-file (name type)
  (string-append (default-ontology-pathname name type)
                 "load."
                 *lisp-suffix*))

;(defun def-ontology-internal2 (name documentation 
;                                    &key (includes (List *base-ontology-name* ))
;                                    (type :domain)
;                                    (pathname (default-ontology-pathname name type))
;                                    author allowed-editors
;                                    (files (default-ontology-files name))
;                                    (select-this-ontology? t))
;  (when (get-ontology name)
;    (warn "Redefining ontology ~S" name)
;    (delete-ontology name))
;  (new-ontology name documentation includes type pathname author allowed-editors
;                files select-this-ontology?))

;(defun def-ontology-internal2 (name documentation 
;                                    &key (includes (List *base-ontology-name* ))
;                                    (type :domain)
;                                    (pathname (default-ontology-pathname name type))
;                                    author allowed-editors
;                                    (files (default-ontology-files name))
;                                    (select-this-ontology? t))
;  (let ((ontology (get-ontology name)))
;    (when ontology
;      (cond ((ontology-included-by ontology)
;             (error 
;              "Cannot redefine ontology ~s, which is used by ontologies ~{~S ~}"
;              name (mapcar #'name (ontology-included-by ontology))))
;            (t
;             (warn "Redefining ontology ~S" name)
;             (delete-ontology name))))
;    (new-ontology name documentation includes  type pathname author allowed-editors
;                  files select-this-ontology?)))

(defun def-ontology-internal2 (name documentation &key
			       includes 
			       (type :domain)
			       (version 1.0)
			       (do-not-include-base-ontology? nil)
			       (pathname (default-ontology-pathname name type))
			       author allowed-editors
			       (files (default-ontology-files name))
			       (select-this-ontology? t)
			       ;; rdf-namespace-label 
			       rdf-namespace-url
			       namespace-uri)
  (let ((ontology (get-ontology name)))
    (when (and (eq includes nil)
	       (eq do-not-include-base-ontology?  nil))
      (setf includes (List *base-ontology-name* )))
    (setf includes (remove-subsumed-ontologies 
		    (mapcar #'get-ontology includes)))
    (if ontology
	(redefine-ontology name ontology documentation includes  type 
			   version pathname 
			   author allowed-editors
			   files select-this-ontology?
			   ;; rdf-namespace-label 
			   rdf-namespace-url
			   :namespace-uri namespace-uri)
	(new-ontology name documentation includes type version pathname
		      author allowed-editors files select-this-ontology?
		      ;; rdf-namespace-label 
		      rdf-namespace-url :namespace-uri namespace-uri))))





(defun new-ontology (name  documentation includes  type version
                           pathname author allowed-editors
                           files select-this-ontology?
                          ;;; rdf-namespace-label 
                           rdf-namespace-url
		     &key namespace-uri)
  
  (let* ((ontology (make-instance 'ocml-ontology
                     :name name
                     :documentation documentation
                     :includes includes
                     :author author
                     :allowed-editors allowed-editors
                     :files files
                    ;;; :rdf-namespace-label rdf-namespace-label 
                     :rdf-namespace-url rdf-namespace-url
                     :version version
                     :pathname pathname
                     :type type
		     :namespace-uri namespace-uri)))
    (finalize-ontology name ontology includes files pathname select-this-ontology? t)))

;(defun redefine-ontology (name ontology  new-documentation new-includes  new-type
;                               new-pathname new-author new-allowed-editors
;                               new-files select-this-ontology?)
;  (with-slots (includes documentation
;                        included-by ontology-type author allowed-editors pathname 
;                        ontology-files
;                        directory)
;              ontology
;    (cond ((set-equal new-includes includes)
;           ;;the ontology has the same structure as before
;           ;;we simply reset its properties and reload its files
;           (setf documentation new-documentation
;                 ontology-type new-type
;                 pathname new-pathname
;                 author new-author
;                 allowed-editors new-allowed-editors
;                 ontology-files new-files
;                 directory (make-ontology-directory))
;           (finalize-ontology name ontology  new-includes new-files new-pathname 
;                              select-this-ontology? nil))
;          (t
;           ;;We need to fully redefine this ontology and all his dependent
;           (delete-ontology name)
;           (new-ontology name new-documentation new-includes new-type new-pathname
;                         new-author new-allowed-editors new-files select-this-ontology?)
;           (loop for dep-onto in included-by
;                 do
;                 (reload-this-ontology-and-its-dependents dep-onto))))))
                
(defun redefine-ontology (name ontology  new-documentation new-includes
			  new-type new-version
			  new-pathname new-author new-allowed-editors
			  new-files select-this-ontology?
			  ;; new-rdf-namespace-label 
			  new-rdf-namespace-url
			  &key
			  namespace-uri)
  (with-slots (includes documentation version
                        ontology-type author allowed-editors pathname 
                        ontology-files
                        directory
			;; rdf-namespace-label 
                        rdf-namespace-url) ontology
    (when (not (string= (namespace-uri-of ontology) namespace-uri))
      (warn "Refusing to change the namespace URI from ~A to ~A."
	    (namespace-uri-of ontology) namespace-uri))
    (mapc #'(lambda (o)                      
              (setf (ontology-included-by o)
                    (remove ontology (ontology-included-by o))))
          includes)
    (setf documentation new-documentation
          version new-version
          includes new-includes
          ontology-type new-type
          pathname new-pathname
          author new-author
          allowed-editors new-allowed-editors
          ontology-files new-files
          directory (make-ontology-directory)
;;;  rdf-namespace-label new-rdf-namespace-label
          rdf-namespace-url new-rdf-namespace-url)
    (finalize-ontology name ontology  new-includes new-files new-pathname 
                       select-this-ontology? nil)
    (reload-dependent-ontologies  
     (sorted-dependent-ontologies ontology)))) ;make sure the order is correct.


(defun reload-dependent-ontologies (dep-ontologies)

  (loop for dep-onto in dep-ontologies
        for new-dep-onto-includes = (remove-subsumed-ontologies 
                                      (ontology-includes dep-onto))
          do
          (with-slots (includes directory) dep-onto
            (mapc #'(lambda (o)                      
                      (setf (ontology-included-by o)
                    (remove dep-onto (ontology-included-by o))))
                includes)
            (setf includes new-dep-onto-includes
                  directory (make-ontology-directory))
            (finalize-ontology (name dep-onto) dep-onto includes 
                               (ontology-files dep-onto)
                               (ontology-pathname dep-onto)
                               nil nil))))


(defun finalize-ontology (name ontology includes files pathname
                               select-this-ontology?
                               new?)
  (setf (ontology-default-fc-rule-packet (ontology-directory ontology))
        (make-default-rule-packet ontology))
  (loop for sub in includes
        do
        (pushnew ontology (ontology-included-by sub)))
  (loop for from-onto in includes
        do
        (copy-ontology from-onto ontology))
  #-:lispworks(record-source-file name 'ocml-ontology)
  #+(or allegro lispworks)(ocml-record-source-file name 'def-ontology name)
  ;;#+:lispworks(record-source-file name 'def-ontology)
  (when new?
    (add-ontology name ontology))
  (load-ontology-files-new  ontology files pathname select-this-ontology?)
  ;;;;;;(maybe-set-rdf-related-information name ontology)
  ontology)


;(defun maybe-set-rdf-related-information (name ontology)
;  (let ((rdf-namespace-label (ontology-rdf-namespace-label ontology)))
;    (unless rdf-namespace-label
;      (setf (ontology-rdf-namespace-label ontology)
;            (string-downcase 
;             (subseq (symbol-name name) 
;                     0 (min 3 (length (symbol-name name)))))))))


(defun copy-ontology (from-ontology to-ontology)
  (let ((from-dir (ontology-directory from-ontology))
        (to-dir (ontology-directory to-ontology)))
  (copy-hash-table (ontology-relations from-dir)
                   (ontology-relations to-dir)
                   from-ontology to-ontology)
  (setf (ontology-axioms to-dir)(ontology-axioms from-dir)
                   )
  (copy-hash-table (ontology-functions from-dir)
                   (ontology-functions to-dir)
                   from-ontology to-ontology)
 ; (copy-hash-table (ontology-operators from-dir)
;                   (ontology-operators to-dir))
  (copy-hash-table (ontology-bc-rules from-dir)
                   (ontology-bc-rules to-dir)
                   from-ontology to-ontology)
  
  (copy-fc-rule-packets from-ontology to-ontology  to-dir)
  (copy-hash-table (ontology-classes from-dir)
                   (ontology-classes to-dir)
                   from-ontology to-ontology)))

(defun copy-fc-rule-packets (from-ontology to-ontology  to-dir)
    (loop for packet in (ontology-rule-packets from-ontology)
          for found = (find-rule-packet-in-ontology (name packet) to-ontology)
          do
          (cond (found
                 (unless (eq (home-ontology found)
                             (home-ontology packet))
                       
	           (ocml-warn
                    "Found conflict when importing definitions
from ontology ~S to ontology ~S.....
a definition for ~A ~S  already exists....
keeping old definition, inherited from ontology ~S"
                    (name from-ontology)
                    (name to-ontology)
                    (type-of found)
                    (name found)
                    (name (home-ontology found)))))
                (t
                 (push packet (ontology-other-fc-rule-packets  to-dir))))))
          

(defun select-most-specific-definition (def-1 def-2 onto-1 onto-2)
  (cond ((member onto-1 (sub-ontologies onto-2))
         def-2)
        ((member onto-2 (sub-ontologies onto-1))
         def-1)))


(defun copy-hash-table (from-table to-table  from-ontology to-ontology )
  (maphash #'(lambda (key value)
               (copy-hash-table-entry  to-table key value  from-ontology to-ontology))
           from-table))

(defun copy-hash-table-entry (to-table key value  from-ontology to-ontology)
  (multiple-value-bind (old flag)
      (gethash key to-table)
    (cond (flag
           (unless (eq (home-ontology value)
                       (home-ontology old))
             (let ((winner (select-most-specific-definition old value (home-ontology old)
                                                            (home-ontology value))))
               (cond (winner
                      (ocml-warn
                         "Found conflict when importing definitions from ontology ~S to ontology ~S.....
a definition for ~A ~S  already exists....keeping most specific definition, inherited from ontology ~S"
                         (name from-ontology)
                         (name to-ontology)
                         (type-of old)
                         key
                         (name (home-ontology winner)))
                        (setf (gethash key to-table)
	                      winner))
                     (t
                      (ocml-warn
                       "Found conflict when importing definitions from ontology ~S to ontology ~S.....
a definition for ~A ~S  already exists....keeping old definition, inherited from ontology ~S"
                       (name from-ontology)
                       (name to-ontology)
                       (type-of old)
                       key
                       (name (home-ontology old))))))))
          (t
	   (setf (gethash key to-table)
	         value)))))
           
(defmacro in-ontology (name)
  `(select-ontology ',name))

(defun select-ontology (name)
  (Let ((ontology (get-ontology name)))
    (unless ontology
      (error "~S is not a defined ontology" name))
    (if (eq ontology *current-ontology*)
      *current-ontology*
      (switch-to-ontology ontology))))

(defun switch-to-ontology (ontology)
  ;;;(save-current-ontology)
  (Let ((dir (ontology-directory ontology)))
    (setf *current-ontology* ontology
          *current-ontologies* (cons *current-ontology*
                                     (sub-ontologies *current-ontology*))
          *defined-relations* (ontology-relations dir)
          *axioms* (ontology-axioms dir)
          *defined-functions* (ontology-functions dir)
          ;;;;(setf *operators* (ontology-operators dir)
          *bc-rules* (ontology-bc-rules dir)
          *domain-classes* (ontology-classes dir))
    ontology))

;(defun save-current-ontology ()
;  (Let ((dir (ontology-directory *current-ontology*)))
;    (setf (ontology-relations dir) *defined-relations*)
;    (setf (ontology-axioms dir) *axioms*)
;    (setf (ontology-functions dir) *defined-functions*)
;    ;;;(setf (ontology-operators dir) *operators*)
;    (setf (ontology-bc-rules dir) *bc-rules*)
;   ;;;; (setf (ontology-fc-rule-packets dir) *rule-packets*)
;     (setf (ontology-classes dir) *domain-classes*)))

(defmacro with-ocml-thread-safety (&body body)
  "Syntactic sugar for CALL-WITH-OCML-THREAD-SAFETY."
  `(call-with-ocml-thread-safety (lambda () ,@body)))

(defun call-with-ocml-thread-safety (closure)
  "Ensure OCML specials are treated indepentently in different
threads."
  (let ((*current-ontology* *current-ontology*)
	(*current-ontologies* *current-ontologies*)
	(*defined-relations* *defined-relations*)
	(*axioms* *axioms*)
	(*defined-functions* *defined-functions*)
	(*bc-rules* *bc-rules*)
	(*domain-classes* *domain-classes*))
    (funcall closure)))

(defmacro with-ontology (ontology &body body)
  "Syntactic sugar for CALL-WITH-ONTOLOGY."
  `(call-with-ontology ,ontology (lambda () ,@body)))

(defun call-with-ontology (ontology closure)
  "Select ONTOLOGY as active OCML ontology for execution of CLOSURE.
ONTOLOGY may be an ontology value, or the name of one.  If
ONTOLOGY is NIL, then restore current ontology on completion of
CLOSURE."
  (with-ocml-thread-safety
      (let ((original-ontology (ocml::name ocml::*current-ontology*)))
	(unwind-protect
	     (progn
	       (when ontology
		 (ocml::select-ontology (if (symbolp ontology)
					    ontology
					    (ocml::name ontology))))
	       (funcall closure))
	  (ocml::select-ontology original-ontology)))))

#-franz-inc
(defun ocml-load (file &key (verbose t) if-does-not-exist)
  (Let ((current-ontology *current-ontology*))
    (unwind-protect 
      (Let ((loaded (Load  file 
                           :verbose verbose 
                           :if-does-not-exist if-does-not-exist)))
        (unless loaded
          (warn "Cannot load file ~a, which does not exist" file)))
      (when current-ontology
        (unless (eq current-ontology *current-ontology*)
          (switch-to-ontology current-ontology))))))


#+franz-inc
(defun ocml-load (file &key (verbose t) if-does-not-exist)
  (Let ((current-ontology *current-ontology*))
    (unwind-protect 
      (Let ((loaded (franz-Load  file 
                           :verbose verbose 
                           :if-does-not-exist if-does-not-exist)))
        (unless loaded
          (warn "Cannot load file ~a, which does not exist" file)))
      (when current-ontology
        (unless (eq current-ontology *current-ontology*)
          (switch-to-ontology current-ontology))))))

#+franz-inc
(defun franz-load (file &key (verbose t)if-does-not-exist)
  (if (and (not (typep file 'pathname)) (find #\; file))
      (let* ((file-name-position (position #\; file :from-end t))
	     (directory (subseq file 0 (1+ file-name-position)))
	     (file-name (subseq file (1+ file-name-position))))
	(load (merge-pathnames (translate-logical-pathname directory)
			       file-name)
	      :verbose verbose :if-does-not-exist if-does-not-exist))
    (Load  file :verbose verbose :if-does-not-exist if-does-not-exist)))


(defun load-base-ontology ()
  #-franz-inc
  (load  (string-append *base-ontology-directory*
                       *base-ontology-load-file*)
         :verbose nil)
  #+franz-inc
  (load (merge-pathnames
	 (translate-logical-pathname *base-ontology-directory*)
	 *base-ontology-load-file*)
        :verbose nil))

(defun load-base-ontology-file (file &key verbose)
  #-franz-inc
  (ocml-load (string-append *base-ontology-directory*
                            file)
             :verbose verbose)
  #+franz-inc
  (ocml-load 
   (merge-pathnames
    (translate-logical-pathname *base-ontology-directory*)
    (string-downcase file))
    :verbose verbose))
  

;  (loop for file in *base-ontology-files*
 ;       with directory = *base-ontology-directory*
  ;      do
   ;     (load (string-append directory file))))

(defun make-ontology-pathname (name type)
  (translate-logical-pathname (string-append
                               *library-pathname*
                               (STRING-DOWNCASE (string type))
                               "s;"
                               (STRING-DOWNCASE (string name))
                               "."
                               *lisp-suffix*)))


;;;;(defun  make-ontology-folder (name type &optional (library "LIBRARY"))
;;;;  (translate-logical-pathname (string-append
;;;;                               "OCML:"
;;;;                               library
;;;;                               ";"
;;;;                               (string-downcase (string type))
;;;;                               "s;"
;;;;                               (string-downcase (string name))
;;;;                               ";")))

;;;ENSURE-ONTOLOGY-INTERNAL ---Ensures that the ontology <name> has been loaded.
(defun ensure-ontology-internal(name type load-file)
  (unless (get-ontology name)
    (unless load-file
      (Setf load-file (default-ontology-load-file name type)))
            ;;;;;;(make-ontology-pathname name type)))
    (ocml-load load-file)))

(defun load-ontology (name type &optional load-file)
  (unless load-file
    (setf load-file (default-ontology-load-file name type)))
  ;;;;;(make-ontology-pathname name type)))
  (ocml-load load-file))

;;(defun load-ontology-files (name type files &optional (dir (string name)) (library "LIBRARY"))
;;  (Loop with dir = (make-ontology-folder dir type library)
;;	for file in files
;;	do
;;	(ocml-load (make-pathname :directory (pathname-directory dir)
;;                                  :type *lisp-suffix*
;;                                  :name file))))


;;;Redefined - Enrico 25/10/00 - to handle constraint checking properly
(defun load-ontology-files-new (ontology files pathname select-this-ontology?)
  (let ((current-ontology *current-ontology*))
    (switch-to-ontology ontology)
    (let ((*pending-constraints*))
      (unwind-protect 
        (Loop 
	  for file in files
	  do
	  (ocml-load (translate-logical-pathname 
                      (string-append pathname file "." *lisp-suffix*)))
          finally
          (loop for c in *pending-constraints*
                do
                (check-pending-constraints c)))
        (unless select-this-ontology?
          (switch-to-ontology current-ontology))))))


;;;CHECK-PENDING-CONSTRAINTS -  Checks the constraint pending from the ontology loading time
;;;That is, it delays checking instance constraints to avoid unnecessary warning - e.g.,
;;;type T of slot S of class C may be defined after an instance of C.
(defun check-pending-constraints (instance-constraint-spec)
  (case (car instance-constraint-spec)
    (:instance-slot-constraints
     (check-slot-assertion-constraints (second instance-constraint-spec)
                                       (third instance-constraint-spec)
                                       (get-slot-values (second instance-constraint-spec)
                                                        (third instance-constraint-spec))
                                       nil
                                       t))
    (:instance-slot-type
     (check-slot-type (second instance-constraint-spec)
                      (third instance-constraint-spec)
                      (get-slot-values (second instance-constraint-spec)
                                       (third instance-constraint-spec))
                      t))
    (:relation-instance-constraints
     (check-relation-instance-constraints (second instance-constraint-spec)
                                          (third instance-constraint-spec)
                                          t))
;    (:instance-constraints
;     (check-instance-constraints (second instance-constraint-spec)
;                                 (parent-class (second instance-constraint-spec))
;                                 t))
    (t
     (error "Internal Error"))))
     
    
    


;;;CLEAR-ONTOLOGY ---Top level function to clear the contents of the current ontology.
;;;If an ontology includes sub-ontologies, then these are NOT cleared.
(defun clear-ontology (&optional (name (name *current-ontology*)))
  (let* ((ontology (get-ontology name))
         (current (unless (eq ontology *current-ontology*)
                    *current-ontology*)))
    (unwind-protect  
      (progn 
        (when current
          (switch-to-ontology ontology))
        (clear-all)
	(loop for sub-onto in (ontology-includes ontology)
	      do
	      (copy-ontology sub-onto ontology)))
      (when current
        (switch-to-ontology current)))))


;;;CLEAR-ALL ---Clears the OCML environment
(defun clear-all ()
  (remove-all-rules :no-sweat :clear-packets)
  (remove-all-relations)
  (remove-all-functions)
  (remove-all-classes)
 )

;;;DELETE-ONTOLOGY - top level function to delete an ontology
(defun delete-ontology (name)
  (Let ((ontology (get-ontology name)))
    (unless ontology
      (error "~S is not a defined ontology" name))
    (if (eq ontology *current-ontology*)
        (if (cdr *all-ontologies*)
            (progn
              (warn "Deleting current ontology..")
              (remove-ontology-internal name ontology)
              (select-ontology (caar *all-ontologies*))
              (ocml-output "~%Current ontology is now ~s"
                           (name *current-ontology*)))
            (error "Cannot delete the only existing ontology"))
        (remove-ontology-internal name ontology))))


;;(defun reconstruct-ontology-system ()
;;  (initialize-ocml))


;;;;;;;;;;;;;;;;;
(defun add-hash-table-entry-to-sub-ontology (to-table key value ontology)
  (multiple-value-bind (old flag)
      (gethash key to-table)
    (cond (flag 
           ;;there is already a value - we only add the new one if it is more specific
           (when (member (home-ontology old)
                         (cons ontology
                            (sub-ontologies ontology)))
             (setf (gethash key to-table)
                   value)))
          (t
           (setf (gethash key to-table)
                   value)))))



(defun propagate-new-def-to-sub-ontologies (name instance type)
  (when *ocml-initialized*
    (loop with ontology = (home-ontology instance)
          with accessor-fun = (if (eq type 'class)
                                'ontology-classes
                                (read-from-string 
                                 (concatenate 'string
                                              "ONTOLOGY-"
                                              (string type)
                                              "S")))
          for dep-onto in (dependent-ontologies ontology)
          for hash-table = (funcall accessor-fun
                                    (ontology-directory dep-onto))
          do
          (add-hash-table-entry-to-sub-ontology hash-table name instance ontology))))

(defun propagate-new-rule-packet-to-sub-ontologies ( instance)
  (loop with ontology = (home-ontology instance)
        for dep-onto in (dependent-ontologies ontology)
       do
        (add-rule-packet-to-dependent-ontology  instance dep-onto ontology)))

(defun add-rule-packet-to-dependent-ontology ( new-packet dep-onto ontology)
  (let* ((name (name new-packet))
         (packet (find-rule-packet-in-ontology name dep-onto)))
    (cond (packet 
           (when (member (home-ontology packet)
                         (cons ontology
                            (sub-ontologies ontology)))
             (remove-rule-packet-from-ontology name dep-onto)
             (push new-packet (ontology-other-fc-rule-packets
                               (ontology-directory dep-onto)))))
          (t
           (push packet (ontology-other-fc-rule-packets
                           (ontology-directory dep-onto)))))))

(defun remove-def-from-dependent-ontologies (name instance type)
  (loop with ontology = (home-ontology instance)
        with accessor-fun = (if (eq type 'class)
                                'ontology-classes
                                (read-from-string 
                                 (concatenate 'string
                                              "ONTOLOGY-"
                                              (string type)
                                              "S")))
          for dep-onto in (sorted-dependent-ontologies ontology)
          for hash-table = (funcall accessor-fun
                                    (ontology-directory dep-onto))
          do
          (remove-hash-table-entry-from-sub-ontology hash-table name   
                                                     accessor-fun ontology dep-onto)))

(defun remove-hash-table-entry-from-sub-ontology (table key  accessor-fun ontology
                                                        dep-onto)
  (multiple-value-bind (value flag)
                       (gethash key table)
    (when (and flag 
               (eq (home-ontology value) ontology))
      (remhash key table)
      (maybe-fetch-definition-from-super-ontologies dep-onto accessor-fun key ontology ))))

(defun maybe-fetch-definition-from-super-ontologies (sub-onto accessor-fun 
                                                            key  super-onto)
  "We have removed <key old-value> from super-onto (and therefore from sub-onto)
   and we now check whether sub-onto can get an other value for <key> from some
   other super ontology"
  (loop for super in  (ontology-includes sub-onto)
        for table = (funcall accessor-fun
                             (ontology-directory super))
        do
        (multiple-value-bind (value flag)
                             (gethash key table)
          (when (and flag 
                     (not (eq (home-ontology value) super-onto)))
            (setf (gethash key (funcall accessor-fun
                                        (ontology-directory sub-onto)))
                  value)
            (return value)))))
