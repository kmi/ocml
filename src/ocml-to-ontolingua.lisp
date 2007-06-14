;;; Mode: Lisp; Package: ocml

;;; The Open University

(in-package "OCML")

(defvar *skip-form-flag*  :$$skip$$)

(defvar *default-ontolingua-sub-directory* "ONTOLINGUA;")

;;;Reminder: This is the syntax accepted by MacLisp
;;;"OCML:library;domains;test-onto;test.lisp"

;(defun translate-ocml-ontology-to-ontolingua (ontology-name-or-def-file)
;  (let ((ontology (get-ontology ontology-name-or-def-file))
;        file)
;    (cond (ontology
;           (translate-ocml-ontology-to-ontolingua-from-load-file
;            (default-ontology-load-file  
;              ontology-name-or-def-file 
;              (ontology-type ontology))))
;          ((or (stringp ontology-name-or-def-file)
;               (streamp ontology-name-or-def-file)
;               (pathnamep ontology-name-or-def-file))
;           (setf file (probe-file ontology-name-or-def-file))
;           (if file
;             (translate-ocml-ontology-to-ontolingua-from-load-file
;              file)
;             (error "Cannot find file ~s" ontology-name-or-def-file)))
;          (t
;           (error "~s is neither an ontology name, nor a known file"
;                  ontology-name-or-def-file)))))

(defun translate-ocml-ontology-to-ontolingua (ontology-name)
  (let ((ontology (get-ontology ontology-name)))
   (cond (ontology
           (translate-ontology-files-to-ontolingua 
            ontology (cons (default-ontology-load-file  
                             ontology-name
                             (ontology-type ontology))
                           (mapcar #'(lambda (file)
                                       (merge-pathnames 
                                        (ontology-pathname ontology)
                                        (make-pathname :name file
                                                       :type *lisp-suffix*)))
                                   (ontology-files ontology)))))
          (t
           (error "~s is not a known ontology" ontology-name)))))

                                     
(defun translate-ontology-files-to-ontolingua (ontology files)
  ;;;maybe select  ontology
  (print "To be written"))



(defun translate-ocml-file-to-ontolingua (source-pathname
                                          &key 
                                          (ontolingua-suffix "onto")
                                          target-pathname)
  (unless target-pathname
    (setf target-pathname
          (merge-pathnames (make-pathname :type ontolingua-suffix) source-pathname)))
  (with-open-file (ifile source-pathname
                         :direction :input)
    (with-open-file (ofile target-pathname
                           :if-exists :supersede
                         :direction :output)
      (format ofile "~%;;;Automatically translated from OCML file ~s"
              source-pathname)
      (loop with new-form
            for form = (read  ifile  nil :eof-value)
            until (eq form :eof-value)
            do
            (setf new-form (translate-ocml-form-into-ontolingua form))
            (unless (eq new-form *skip-form-flag*)
              (format ofile "~%")
              (princ new-form ofile))
            (format ofile "~%")))))
    
(defun translate-ocml-form-into-ontolingua (form)
  (cond ((in-package? form)
         (concatenate 'string 
                      "(in-package "
                      (write-to-string "ONTOLINGUA-USER")
                      ")"))
        ((in-ontology? form)
         (concatenate 'string "(in-ontology '" (write-to-string (second form)
                                                                      :case :downcase)
                      ")"))
        ((def-class-form? form)
         (translate-def-class-form form))))

(defun translate-def-class-form (form)
  (destructuring-bind (def-class name &optional superclasses  instance-var  documentation
                                 class-slots &rest relation-spec)
                      form
    def-class ;;ignore
   ;; (print (list name superclasses instance-var documentation))
     (multiple-value-bind (instance-var1  documentation1 class-slots1 relation-spec1)
      (parse-class-form instance-var  documentation class-slots relation-spec)
       (let (;;;;(supers (translate-superclasses superclasses ))
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
       ")"
       )))))
       
   
    

        


(defun translate-class-slots (name class-slots)
  (mapcar #'(lambda (spec)
                (translate-each-slot-spec name spec))
            (parse-class-slots name class-slots)))


(defun translate-each-slot-spec ( name spec)
  (destructuring-bind (slot ocml-options clos-options) spec
    clos-options ;;ignore
    (format nil "(~:(~s ~)~{~%~a~})"
             slot
             (mapcar #'(lambda (option)
                          (translate-class-slot-option name option))
                          ocml-options))))

(defun translate-class-slot-option (name option)
  (let ((result 
         (case (car option)
           (:type 
            `(value-type ,(second option)))
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
  (and (listp form)
       (= (length form) 2)
       (eq (car form) 'in-ontology)
       (symbolp (second form))))

(defun in-package? (form)
  (and (listp form)
       (= (length form) 2)
       (eq (car form) 'in-package)))

(defun DEF-CLASS-FORM? (form)
  (and (listp form)
       (eq (car form) 'def-class)))

    