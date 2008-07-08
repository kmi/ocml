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
               (:file "ask-tell" :depends-on ("defpackage" "globals"))
               (:file "axioms" :depends-on ("base" "defpackage" "globals"))
               ;; XXX (:file "back-chain-compiler" :depends-on ("defpackage"))
               (:file "back-chain" :depends-on
                      ("defpackage" "basic" "relations" "rules" "globals"))
               (:file "base" :depends-on ("defpackage"))
               (:file "basic" :depends-on ("defpackage" "globals" "theories"))
               (:file "constraints" :depends-on ("defpackage"))
               (:file "control" :depends-on
                      ("defpackage" "basic" "domain" "io" "parser"
                                    "slot-renaming" "top"))
               (:file "delete-things" :depends-on ("defpackage" "relations"))
               (:file "describe" :depends-on ("defpackage"))
               (:file "domain" :depends-on ("defpackage" "globals"))
               (:file "forward-chain" :depends-on ("defpackage" "rules"))
               (:file "forward-chain-call" :depends-on ("defpackage"))
               (:file "functions" :depends-on ("defpackage" "globals"))
               (:file "globals" :depends-on ("defpackage" "utilities"))
               (:file "mapping" :depends-on ("defpackage"))
               (:file "match" :depends-on ("defpackage"))
               (:file "meta" :depends-on ("defpackage"))
               (:file "namespaces" :depends-on ("defpackage" "globals"))
               (:file "io" :depends-on ("defpackage" "globals"))
               (:file "ocml-to-ontolingua" :depends-on ("defpackage" "globals"))
               (:file "ocml-to-owl" :depends-on ("defpackage" "theories"))
               (:file "ocml-to-rdfs" :depends-on ("defpackage" "globals" "theories"))
               (:file "parser" :depends-on ("defpackage"))
               (:file "relations" :depends-on ("defpackage" "rules" "globals"))
               (:file "rete" :depends-on ("defpackage"))
               (:file "rete-working-memory" :depends-on ("defpackage"))
               (:file "rules" :depends-on ("basic" "defpackage" "theories"))
               (:file "slot-renaming" :depends-on ("defpackage"))
               (:file "theories" :depends-on ("defpackage" "base" "globals"))
               (:file "top" :depends-on ("defpackage"))
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
	     (:file "translation" :depends-on ("defpackage" "sundry"))))))

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
