(defpackage ocml-system
  (:use :common-lisp :asdf))

(in-package :ocml-system)

;; Enable source location recording in Lispworks 5.
#+:lispworks5  (pushnew :lispworks-dspec *features*)

(defsystem :ocml
    :description "Operational Concept Modelling Language."
    :version "7.5"
    :author "John Domingue <j.b.domingue@open.ac.uk> and Enrico Motta <e.motta@open.ac.uk>"
    :depends-on (:ocml-pre-webonto :ocml-post-webonto))

(defsystem :ocml-pre-webonto
  :description "OCML before loading WebOnto."
  :depends-on (:enrico-utilities)
  :components
  ((:module :src
	    :components
	    ((:file "defpackage")
	     (:file "axioms" :depends-on ("defpackage" "globals"))
	     (:file "backwrd21" :depends-on ("defpackage"
					     "basic21"
					     "rels8" "rules4" "globals"))
	     (:file "basic21" :depends-on ("defpackage" "globals"))
	     (:file "constrs2" :depends-on ("defpackage"))
	     (:file "delete-things" :depends-on ("defpackage" "rels8"))
	     (:file "describe" :depends-on ("defpackage"))
	     (:file "domain6" :depends-on ("defpackage" "globals"))
	     (:file "fc" :depends-on ("defpackage" "rules4"))
	     (:file "fc-call3" :depends-on ("defpackage"))
	     (:file "funs7" :depends-on ("defpackage" "globals"))
	     (:file "io" :depends-on ("defpackage" "globals"))
	     (:file "mapping4" :depends-on ("defpackage"))
	     (:file "match3" :depends-on ("defpackage"))
	     (:file "meta" :depends-on ("defpackage"))
	     (:file "namespaces" :depends-on ("defpackage" "globals"))
	     (:file "ocml-to-ontolingua2" :depends-on ("defpackage" "globals"))
	     (:file "ocml-to-owl" :depends-on ("defpackage"))
	     (:file "ocml-to-rdfs" :depends-on ("defpackage" "globals"))
	     (:file "parser5" :depends-on ("defpackage"))
	     (:file "rels8" :depends-on ("defpackage" "rules4" "globals"))
	     (:file "rete4" :depends-on ("defpackage"))
	     (:file "rules4" :depends-on ("defpackage"))
	     (:file "slot-renaming3" :depends-on ("defpackage"))
	     (:file "tellask4" :depends-on ("defpackage"))
	     (:file "theories5" :depends-on ("defpackage" "globals"))
	     (:file "top9" :depends-on ("defpackage"))
	     (:file "globals" :depends-on ("defpackage"))
	     (:file "wm2" :depends-on ("defpackage"))
	     ;; XXX (:file "backcmp2" :depends-on ("defpackage"))
	     ))))

;;; Separate out control4 to enable redefinition of ocml-metaclass
;;; john domingue may 21 98
(defsystem :ocml-post-webonto
    :depends-on (:ocml-pre-webonto)
    :components ((:module :src :components
			  ((:file "control4")))))

;;; XXX The XML/XSD translators depends on the XML parser in the IRS.
;;; For the moment, we'll pretend that the IRS ASDF package is
;;; identical with that having been loaded.
(defsystem :ocml-xml
    :depends-on (:ocml :irs)
    :components ((:module :src :components
			  ((:file "xml-to-ocml")
			   ;; (:file "xsd-to-ocml")
			   ))))

(defsystem ocml-tests
  :depends-on (:ocml :fiveam)
  :components
  ((:module :tests :components
	    ((:file "defpackage")
	     (:file "setup" :depends-on ("defpackage"))
	     (:file "apples-suite" :depends-on ("defpackage" "setup"))
	     (:file "namespaces" :depends-on ("defpackage" "setup"))
	     (:file "owl-suite" :depends-on ("defpackage" "setup"))))))

(defsystem :ocml-xml-tests
    :depends-on (:ocml-xml :ocml-tests)
    :components
    ((:module :tests :components
	      ((:file "xml-to-ocml")
	       ;; (:file "xsd-to-ocml")
	       ))))

(eval-when (:execute :load-toplevel)
  (handler-case (logical-pathname-translations "ocml")
    ;; ANSI says simple-type-error, but some folks can't seem to read
    ;; :-|
    ( #-(or :allegro :lispworks) simple-type-error
      #+:allegro type-error
      #+:lispworks simple-error (e)
      (declare (ignore e))
      (setf (logical-pathname-translations "ocml")
	    `(("ocml:library;**;*.*.*"
	       ,(format nil "~Alibrary/**/*.*"
			(asdf:component-pathname (asdf:find-system :ocml)))))))))

