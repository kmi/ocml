;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(eval-when (eval load)
  (setf *ocml-version* "3.1"))

(defun initialize-ocml ()
  (ocml-output "~%Initializing OCML......"
               *ocml-version*)
  (switch-to-ontology *base-ontology*)
  (clear-all)
  (load-base-ontology))
  ;;;;(clear-all)
  


(defun welcome ()
  (ocml-output 
   "~2%Welcome to OCML version ~A.~%"
          *ocml-version*)
  (values))

(eval-when (eval load)
  (welcome)
  (initialize-ocml))



