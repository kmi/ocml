;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defun initialize-ocml (&optional (load-base-ontology? t))
  (ocml-output "~%Initializing OCML......"
               *ocml-version*)
   (setf *all-ontologies* nil)
   (setf *ocml-initialized* t)
   (when load-base-ontology?
     (load-base-ontology)))

(defun welcome ()
  ;;;this is now switched off when loading OCML, I reset  it here
  ;;; - Enrico 15/2/99
  ;;;;;;(setf *ignore-undefined-relations* nil)
  ;;;;Now the default is :warn - 17/7/00
  (ocml-output 
   "~2%Welcome to OCML version ~A.~%"
          *ocml-version*)
  (values))


;;;still want to do this on the mac - Enrico 15/2/99
#+(or :mcl 
      :HARLEQUIN-PC-LISP)
(eval-when (eval load)
  (welcome)
  #-:webonto (initialize-ocml )
  )

;;;do this later because i redefine some ocml classes john domingue may 21 98
;;;include some versions of functions in the user package to aid with system
;;;loading

(defun cl-user::welcome ()
  (welcome))


(defun cl-user::initialize-ocml ()
  (initialize-ocml))

;;;LOAD-OCML ---Imports in the OCML package the cl-user definition -Enrico 17/2/99
(defun load-ocml (&optional compile? force?)
  (cl-user::load-ocml compile? force?))

