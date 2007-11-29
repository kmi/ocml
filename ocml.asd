(defpackage ocml-system
  (:use :common-lisp :asdf))

(in-package :ocml-system)

;; Enable source location recording in Lispworks 5.
#+:lispworks5  (pushnew :lispworks-dspec *features*)

(defsystem :ocml
    :description "Operational Concept Modelling Language."
    :version "7.5"
    :author "Enrico Motta <e.motta@open.ac.uk> et al at the Open University."
    :components
    ((:module :src
	    :components
	    ((:file "defpackage")
	     (:file "axioms" :depends-on ("defpackage" "globals"))
	     ;; XXX (:file "backcmp2" :depends-on ("defpackage"))
	     (:file "backwrd21" :depends-on
		    ("defpackage" "basic21" "rels8" "rules4" "globals"))
	     (:file "basic21" :depends-on ("defpackage" "globals" "theories5"))
	     (:file "constrs2" :depends-on ("defpackage"))
             (:file "control4" :depends-on ("defpackage" "basic21" "parser5" "io"
                                                         "slot-renaming3" "top9"))
	     (:file "delete-things" :depends-on ("defpackage" "rels8"))
	     (:file "describe" :depends-on ("defpackage"))
	     (:file "domain6" :depends-on ("defpackage" "globals"))
	     (:file "fc" :depends-on ("defpackage" "rules4"))
	     (:file "fc-call3" :depends-on ("defpackage"))
	     (:file "funs7" :depends-on ("defpackage" "globals"))
	     (:file "globals" :depends-on ("defpackage" "utilities"))
	     (:file "mapping4" :depends-on ("defpackage"))
	     (:file "match3" :depends-on ("defpackage"))
	     (:file "meta" :depends-on ("defpackage"))
	     (:file "namespaces" :depends-on ("defpackage" "globals"))
	     (:file "io" :depends-on ("defpackage" "globals"))
	     (:file "ocml-to-ontolingua2" :depends-on ("defpackage" "globals"))
	     (:file "ocml-to-owl" :depends-on ("defpackage" "theories5"))
	     (:file "ocml-to-rdfs" :depends-on ("defpackage" "globals" "theories5"))
	     (:file "parser5" :depends-on ("defpackage"))
	     (:file "rels8" :depends-on ("defpackage" "rules4" "globals"))
	     (:file "rete4" :depends-on ("defpackage"))
	     (:file "rules4" :depends-on ("defpackage"))
	     (:file "slot-renaming3" :depends-on ("defpackage"))
	     (:file "tellask4" :depends-on ("defpackage"))
	     (:file "theories5" :depends-on ("defpackage" "globals"))
	     (:file "top9" :depends-on ("defpackage"))
	     (:file "wm2" :depends-on ("defpackage"))
	     (:file "utilities" :depends-on ("defpackage"))))))

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
	     (:file "concepts" :depends-on ("defpackage"))
	     (:file "constraints" :depends-on ("defpackage" "sundry"))
	     (:file "sundry" :depends-on ("defpackage"))
	     (:file "apples-suite" :depends-on ("defpackage" "sundry"))
	     (:file "namespaces" :depends-on ("defpackage" "sundry"))
	     (:file "owl-suite" :depends-on ("defpackage" "sundry"))))))

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
