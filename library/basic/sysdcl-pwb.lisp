;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: COMMON-LISP-USER;   -*-

(in-package "COMMON-LISP-USER")


;;;This file sets the logical pathnames translations and needs to 
;;;be customised for each site.  Allegro does not support 
;;;logical pathnames, so the customization is slightly different.

 

;;;Unless you are using allegro, you have to modify this form to set the mapping 
;;;between logical and physical pathnames in your site
#+:mcl
(eval-when (eval compile load)
  (setf (logical-pathname-translations "OCML") 
      `(("EXPERIMENTAL;**;*" "Macintosh HD:Enrico:code:ocml:versions:v7.0:**:*")
        ("RELEASED;**;*" "Macintosh HD:Enrico:code:ocml:versions:v7.0:**:*")
        ("LIBRARY;**;*" "Macintosh HD:Enrico:code:ocml:library:v5.0:**:*"))))

#+(and :LISPWORKS
       (not :HARLEQUIN-PC-LISP))
(eval-when (eval compile load)
  (setf (logical-pathname-translations "OCML")
      `(("EXPERIMENTAL;**;*" 
         ,(translate-logical-pathname "food:lisp;ocml;**;*")) 
        ("RELEASED;**;*" 
         ,(translate-logical-pathname "food:lisp;ocml;**;*"))
	("LIBRARY;**;*" 
         ,(translate-logical-pathname "food:lisp;ocml;library;**;*")))))



#+:HARLEQUIN-PC-LISP
(eval-when (eval compile load)
  (setf (logical-pathname-translations "OCML")
      '(("EXPERIMENTAL;**;*" "C:\\users\\em5\\code\\ocml\\versions\\v7.0\\**\\*")
        ("RELEASED;**;*" "C:\\users\\em5\\code\\ocml\\versions\\v7.0\\**\\*")
        ("LIBRARY;**;*" "C:\\users\\em5\\code\\ocml\\library\\v5.0\\**\\*"))))


#+:allegro
(eval-when (eval compile load)
  (setf (logical-pathname-translations "OCML")
      `(("EXPERIMENTAL;**;*" 
         ,(translate-logical-pathname "food:lisp;ocml;**;*"))
	("experimental;**;*" 
         ,(translate-logical-pathname "food:lisp;ocml;**;*"))
        ("RELEASED;**;*" 
         ,(translate-logical-pathname "food:lisp;ocml;**;*"))
	("released;**;*" 
         ,(translate-logical-pathname "food:lisp;ocml;**;*"))
	("LIBRARY;**;*" 
         ,(translate-logical-pathname "food:lisp;ocml;library;**;*"))
	("library;**;*" 
         ,(translate-logical-pathname "food:lisp;ocml;library;**;*")))))


           

(defvar *lisp-suffix*)

(eval-when (eval load)
  #+:LISPWORKS (setf *lisp-suffix* "lisp")
  #+:allegro (setf *lisp-suffix* "lisp")
  #+:mcl(setf *lisp-suffix* "lisp")
  #+:windows(setf *lisp-suffix* "lisp"))

(defvar *binary-suffix*)
  

(eval-when (eval load)
  #+:HARLEQUIN-PC-LISP (setf *binary-suffix* "fsl")
  #+allegro(setf *binary-suffix* "fasl")
  #+:mcl(setf *binary-suffix* "pfsl"))  


#-:franz-inc 
(eval-when (eval compile load)
  (defparameter *source-pathname* "OCML:EXPERIMENTAL;SOURCE;")
  (defparameter *utilities-pathname* "OCML:EXPERIMENTAL;UTIL;")
  ;;;;;(defparameter *examples-pathname* "OCML:EXAMPLES;")
  (load (merge-pathnames *utilities-pathname* "sysdcl")))


;;;PACKAGE OCML
(defpackage ocml
  (:use 
   "COMMON-LISP" "UTILITIES"))

(import '(*source-pathname* *utilities-pathname*  
         ;;;*examples-pathname*
          *lisp-suffix*)
        (find-package "OCML"))



(defun compile-file-if-necessary (file)
  (Let ((flag (or (not (probe-file (concatenate 'string file "." *binary-suffix*)))
                  (and (< (file-write-date (concatenate 'string file "." *binary-suffix*))
                          (file-write-date (concatenate 'string file "." *lisp-suffix*)))))))
    (when flag
      (compile-file  file :verbose t))))


;;;If we are running lispworks on unix 
#+(and :LISPWORKS
       (not :HARLEQUIN-PC-LISP))
(defsystem ocml
  (
   :default-pathname  *source-pathname*
   :package ocml
   :default-type :lisp-file)
   :members (("ocml-utilities" :type :system)
                "vars4"
                 "io"
                 "match3"
		 "rels4"
		 "meta"
                 "funs7"
		 "basic20"
                 "domain5"
                 "rules4"
                 "parser5"
                 "tellask4"
		 "backwrd21"
                 "top9"
                 "fc"
                 "rete4"
                 "wm2"
                 "fc-call3"
                 "theories5"
                 ;;;"compiler11"
                 "compiler18"
                 "axioms"
                 "mapping4"
                 "backcmp2"
                 "describe"
                 "slot-renaming3"
                 "control4"
                 "delete-things"
                 "constrs2"
                 "ocml-to-ontolingua2"
                 "patches2"
                 )
   :rules ((:In-order-to :compile :all
                        (:requires (:Load :Previous)))))

;;;separate out control4 to enable redefinition of ocml-metaclass john domingue may 21 98 
#+(and :LISPWORKS
       (not :HARLEQUIN-PC-LISP))
(defsystem ocml-after-web-onto
  (
   :default-pathname  *source-pathname*
   :package ocml
   :default-type :lisp-file)
   :members
   ("control4")
   :rules((:In-order-to :compile :all
                        (:requires (:Load :Previous)))))


;;;If we are running lispworks on unix 
#+:HARLEQUIN-PC-LISP
(defsystem ocml
  (
   :default-pathname  *source-pathname*
   :package ocml
   :default-type :lisp-file)
   :members (("ocml-utilities" :type :system)
                "vars4"
                 "io"
                 "match3"
		 "rels4"
		 "meta"
                 "funs7"
		 "basic20"
                 "domain5"
                 "rules4"
                 "parser5"
                 "tellask4"
		 "backwrd21"
                 "top9"
                 "fc"
                 "rete4"
                 "wm2"
                 "fc-call3"
                 "theories5"
                 ;;;"compiler11"
                 "compiler18"
                 "axioms"
                 "mapping4"
                 "backcmp2"
                 "describe"
                 "slot-renaming3"
                 "control4"
                 "delete-things"
                 "constrs2"
                 "ocml-to-ontolingua2"
                 "patches2"
             )
   :rules ((:In-order-to :compile :all
                        (:requires (:Load :Previous)))))

#+allegro
(defsys ocml 
  "food:lisp;ocml;source;"
  ocml-utilities
            "vars4"
                 "io"
                 "match3"
		 "rels4"
		 "meta"
                 "funs7"
		 "basic20"
                 "domain5"
                 "rules4"
                 "parser5"
                 "tellask4"
		 "backwrd21"
                 "top9"
                 "fc"
                 "rete4"
                 "wm2"
                 "fc-call3"
                 "theories5"
                 ;;;"compiler11"
                 "compiler18"
                 "axioms"
                 "mapping4"
                 "backcmp2"
                 "describe"
                 "slot-renaming3"
                 "control4"
                 "delete-things"
                 "constrs2"
                 "ocml-to-ontolingua2"
                 "patches2"
  )

;;;separate out control4 to enable redefinition of ocml-metaclass john domingue may 21 98 
#+allegro
(defsys ocml-after-web-onto
  "food:lisp;ocml;source;"
  "control4")

#+:LISPWORKS
(defun load-ocml (&optional compile? force?)
  (if compile?
      (defsystem:compile-system 'ocml :load t :force force?)
      (defsystem:load-system 'ocml )))

;;;If we are not using lispworks, then we load the files explicitly, one by one.
;;;Allegro doesn't seem to have a good file system interface, so the allegro-specific
;;;code is a bit messy

#-:LISPWORKS
(defun load-ocml (&optional compile? force?)
  (Let ((fun  #-:franz-inc (if force?
                              #'(lambda (x)(compile-file x)(load x :verbose t))
                              (if compile?
                                #'(lambda (x)(compile-file-if-necessary x)(load x :verbose t))
                                #'(lambda (x) (load x :verbose t))))
         #+:franz-inc (if compile? 
                         #'(lambda (x) 
                              (compile-file 
                                 (concatenate 'string
                                  x ".lsp")) 
                              (load (concatenate 
                                       'string
                                     x ".fsl")
                                 :verbose t))
                         #'(lambda (x) (or (load (concatenate 
                                                    'string
                                                  x ".fsl")
                                              :verbose t
                                              :if-does-not-exist nil)
                                           (load (concatenate 
                                                    'string
                                                  x ".lsp")
                                              :verbose t
                                              ))))))
     (map nil fun 
        (cons 
           (concatenate 'string *utilities-pathname* "cl-util")
           (mapcar #'(lambda (file)
                         (concatenate 'string *source-pathname* file))
               '(
                 "vars4"
                 "io"
                 "match3"
		 "rels4"
		 "meta"
                 "funs7"
		 "basic20"
                 "domain5"
                 "rules4"
                 "parser5"
                 "tellask4"
		 "backwrd21"
                 "top9"
                 "fc"
                 "rete4"
                 "wm2"
                 "fc-call3"
                 "theories5"
                 ;;;"compiler11"
                 "compiler18"
                 "axioms"
                 "mapping4"
                 "backcmp2"
                 "describe"
                 "slot-renaming3"
                 "control4"
                 "delete-things"
                 "constrs2"
                 "ocml-to-ontolingua2"
                 "patches2"))))))



 
                          
                       
