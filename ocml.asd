;;; Copyright © 2007,2008 The Open University

(defpackage #:ocml-system
  (:use :common-lisp :asdf))

(in-package #:ocml-system)

;;; We need CLISP to be as ANSI compatible as it can be.  In
;;; particular, not doing this breaks a lot of logical pathname stuff.
#+:clisp (setf custom:*ansi* t)

;;; Enable source location recording in Lispworks 5.
#+:lispworks5 (pushnew :lispworks-dspec *features*)

;;; Check for availablility of the Drakma HTTP client.
(when (asdf:find-system :drakma nil)
  (push :ocml-with-drakma *features*))

;;; Check for availablility of the Closure XML library.
(when (asdf:find-system :cxml nil)
  (push :ocml-with-cxml *features*))

(defsystem :ocml
    :description "Operational Concept Modelling Language."
    :version "7.5"
    :author "Enrico Motta <e.motta@open.ac.uk> et al at the Open University."
    :depends-on (#+:ocml-with-cxml :cxml #+:ocml-with-drakma :drakma)
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
                      ("defpackage" "basic" "domain" "io" "parser" "relations"
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
               (:file "ocml-to-ontolingua" :depends-on ("defpackage" "globals" "theories"))
               (:file "ocml-to-owl" :depends-on ("defpackage" "theories"))
               (:file "ocml-to-rdfs" :depends-on ("defpackage" "globals" "theories"))
               (:file "parser" :depends-on ("defpackage" "match"))
               (:file "relations" :depends-on ("defpackage" "rules" "globals"))
               (:file "rete" :depends-on ("defpackage"))
               (:file "rete-working-memory" :depends-on ("defpackage"))
               (:file "rules" :depends-on ("basic" "defpackage" "theories"))
               (:file "slot-renaming" :depends-on ("defpackage"))
               (:file "theories" :depends-on ("defpackage" "base" "globals"))
               (:file "top" :depends-on ("defpackage"))
               (:file "utilities" :depends-on ("defpackage"))
               #+:ocml-with-cxml
               (:file "xml-to-ocml" :depends-on ("defpackage"))
               ;; Isn't working at the moment...
               ;; #+:ocml-with-cxml
               ;; (:file "xsd-to-ocml" :depends-on ("defpackage"))
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
	     (:file "translation" :depends-on ("defpackage" "sundry"))
             #+:ocml-with-cxml
             (:file "xml-to-ocml" :depends-on ("defpackage"))))))

(eval-when (:execute :load-toplevel)
  (handler-case (logical-pathname-translations "ocml")
    ;; ANSI says simple-type-error, but some folks can't seem to read
    ;; :-|
    ( #-(or :allegro :clisp :lispworks) simple-type-error
      #+:allegro type-error
      #+:clisp simple-error
      #+:lispworks simple-error
      (e)
      (declare (ignore e))
      (setf (logical-pathname-translations "ocml")
	    `(("ocml:library;**;*.*.*"
	       ,(format nil "~Alibrary/**/*.*"
			(asdf:component-pathname (asdf:find-system :ocml)))))))))
