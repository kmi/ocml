(defpackage ocml-system
  (:use :common-lisp :asdf))

(in-package :ocml-system)

(defsystem :ocml
    :description "Operational Concept Modelling Language."
    :version "7.3.epsilon"
    :author "John Domingue <j.b.domingue@open.ac.uk> and Enrico Motta <e.motta@open.ac.uk>"
    :depends-on (:ocml-pre-webonto :ocml-post-webonto))

;;; Unless you are using allegro, you have to modify this form to set
;;; the mapping between logical and physical pathnames in your site
#|
(eval-when (eval compile load)
  (setf (logical-pathname-translations "OCML") 
	#+:mcl
	`(("LIBRARY;**;*" "Enrico's Stuff:Enrico:code:ocml:library:v5.0:**:*"))
	#+:lispworks
	`(("LIBRARY;**;*" ,(translate-logical-pathname "food:lisp;ocml;library;**;*")))))
|#

(defsystem :ocml-pre-webonto
  :description "OCML before loading WebOnto."
  :depends-on (:enrico-utilities)
  :components
  ((:module :src
	    :components
	    ((:file "defpackage")
	     (:file "axioms" :depends-on ("defpackage" "vars5"))
	     (:file "backwrd21" :depends-on ("defpackage"
					     "basic21"
					     "rels8" "rules4" "vars5"))
	     (:file "basic21" :depends-on ("defpackage" "vars5"))
	     (:file "constrs2" :depends-on ("defpackage"))
	     (:file "delete-things" :depends-on ("defpackage" "rels8"))
	     (:file "describe" :depends-on ("defpackage"))
	     (:file "domain6" :depends-on ("defpackage" "vars5"))
	     (:file "fc" :depends-on ("defpackage" "rules4"))
	     (:file "fc-call3" :depends-on ("defpackage"))
	     (:file "funs7" :depends-on ("defpackage" "vars5"))
	     (:file "io" :depends-on ("defpackage" "vars5"))
	     (:file "mapping4" :depends-on ("defpackage"))
	     (:file "match3" :depends-on ("defpackage"))
	     (:file "meta" :depends-on ("defpackage"))
	     (:file "namespaces" :depends-on ("defpackage" "vars5"))
	     (:file "ocml-to-ontolingua2" :depends-on ("defpackage" "vars5"))
	     (:file "ocml-to-rdfs7" :depends-on ("defpackage" "vars5"))
	     (:file "owl" :depends-on ("defpackage"))
	     (:file "parser5" :depends-on ("defpackage"))
	     (:file "patches2" :depends-on ("defpackage" "vars5"))
	     (:file "rels8" :depends-on ("defpackage" "rules4" "vars5"))
	     (:file "rete4" :depends-on ("defpackage"))
	     (:file "rules4" :depends-on ("defpackage"))
	     (:file "slot-renaming3" :depends-on ("defpackage"))
	     (:file "tellask4" :depends-on ("defpackage"))
	     (:file "theories5" :depends-on ("defpackage" "vars5"))
	     (:file "top9" :depends-on ("defpackage"))
	     (:file "vars5" :depends-on ("defpackage"))
	     (:file "wm2" :depends-on ("defpackage"))
	     ;; XXX (:file "backcmp2" :depends-on ("defpackage"))
	     ))))

;;; Separate out control4 to enable redefinition of ocml-metaclass
;;; john domingue may 21 98
(defsystem :ocml-post-webonto
    :depends-on (:ocml-pre-webonto)
    :components ((:module :src :components
			  ((:file "control4")))))

(defsystem ocml-tests
  :depends-on (:ocml :fiveam)
  :components
  ((:module :tests :components
	    ((:file "defpackage")
	     (:file "setup" :depends-on ("defpackage"))
	     (:file "apples-suite" :depends-on ("defpackage" "setup"))
	     (:file "namespaces" :depends-on ("defpackage" "setup"))
	     (:file "owl-suite" :depends-on ("defpackage" "setup"))))))

(eval-when (:execute :load-toplevel)
  (handler-case (logical-pathname-translations "ocml")
    (simple-error (e)
      (setf (logical-pathname-translations "ocml")
	    `(("ocml:library;basic;**;*"
	       ,(format nil "~Alibrary/basic/"
			(asdf:component-pathname (asdf:find-system :ocml)))))))))

