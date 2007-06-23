;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defvar *ocml-version* "7.4")

(defvar *library-pathname* "OCML:LIBRARY;")

(defvar *lisp-suffix* "lisp")

(defvar *binary-suffix*
  #+(and :lispworks :win32) "fsl"
  #+(and :lispworks :linux) "ufasl"
  #+:allegro "fasl"
  #+:mcl "pfsl")

(defvar *ocml-top-class* 'ocml-thing)

(defconstant *subclass-of-slot* 'subclass-of)
(defconstant *instance-of-slot* 'instance-of)

(defconstant *legal-ocml-slot-options*
  '(:default-value :value :type  :inheritance :min-cardinality 
  :max-cardinality :cardinality :documentation))

(defconstant *slot-info-options*
  '(:default-value :value :type  :inheritance :min-cardinality 
  :max-cardinality ))

(defconstant *class-spec-lisp-options*
  '(:lisp-slots :lisp-class-name))

(defconstant *class-spec-slot-related-options* '( :own-slots :slot-renaming))

(defconstant *relation-spec-keywords*
  '( :sufficient :iff-def :prove-by :exclusive-prove-by :constraint 
     :no-proofs-by 
     :sufficient-for-type-checking 
     :lisp-fun))

(defconstant *ALL-CLASS-DEFINITION-LEGAL-OPTIONS*
  (append *class-spec-slot-related-options* *class-spec-lisp-options*
          *relation-spec-keywords*
          '(:avoid-infinite-loop)))
          

(defconstant *value-options*
  '(:default-value :value))

(defconstant *non-value-options*
  '( :type :min-cardinality
    :max-cardinality))



(defvar *default-inheritance* :supersede)

(defconstant *inheritance-options* '(:merge :supersede))

(defconstant *legal-clos-slot-options*
  '(:reader :writer :accessor :allocation :initarg :initform ;;;:type
    ;;;;:documentation
    ))

(defvar *current-environment*)

(defvar *ignore-undefined-relations* :warn
  "When this variable is NIL, if we try to prove a goal (rel t1,...,tn) and
   rel has not been defined, an error is signalled.  If the value is :WARN, then
   a warning is issued.  If the value is T, then the undefined relation is ignored")

(defvar *warn-about-undefined-types* nil)

(defvar *warn-about-free-vars-in-rhs?*)

(defvar *tell-operators* '(tell))

(defvar *current-application* nil "The application currently being executed")

(defvar *interpreter-running* nil
  "Is T when the forward chainer interpreter is running")

(defvar *compiling-fc-rule*  nil
  "Is T when we are compiling a fc-rule")

(defvar *check-cardinality* nil
  "If this variable is set to T we check that instances slot comply with cardinality constraints")

;;;This variable is used to store failed constraints when loading an ontology
;;;in case such constraints can be satisfied after the full ontology is loaded
(defvar *pending-constraints*)

(defvar *asserting-own-slots* nil
  "T when we are asserting the own slots of a class")

(defvar *check-constraints* nil
  "When T OCML will check that instance, slot or relation assertions satisfy applicable constraints")

(defvar *ocml-initialized* NIL "NIL until OCML is initialized")

;;;*FC-IN-WATCHER-MODE*
;;When in watcher mode, fc rules run as soon as something is asserted which 
;;;triggers them, rather than as a result of an explicit 'run' command
(defvar *fc-in-watcher-mode* nil)


(defvar *default-fc-rule-priority* :normal)
(defvar *fc-priorities* '(:normal :high :low))


(defvar *trace-fc* nil)
(defvar *traced-assertions* nil)
(defvar *trace-tasks* nil)

(defvar *task-level* 0)

(defvar *default-mapping-relation* 'maps-to)
;
;#-:franz-inc 
;(defparameter *basic-rels-file* (string-append *examples-pathname*
;                                         "BASIC;baserels3." *lisp-suffix*))
;#-:franz-inc 
;(defparameter *basic-funs-file* (string-append *examples-pathname*
;                                         "BASIC;basefuns2."*lisp-suffix*))
;#-:franz-inc 
;(defparameter *basic-ops-file* (string-append *examples-pathname*
;                                         "BASIC;baseops2." *lisp-suffix*))
;#-:franz-inc 
;(defparameter *basic-classes-file* (string-append *examples-pathname*
;                                         "BASIC;baseclas2."  *lisp-suffix*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *nothing* :nothing
  "The constant returned by an IF expression which does not evaluate its then
  or else parts")

(defvar *default-eval-args* nil
  "A variable to specify whether lists should be treated as function applications or not.
   By default this is the case in functional expressions - e.g IF, CALL, etc -
   and it is not in rules.")

(defparameter *hardwired-functions*
    '(quote if cond in-environment findall 
            the setofall eval apply call);;;;rcall)
            ;;;;;call   set-of list-of)
  "The OCML functions whose definition is 'hardwired'")

(defparameter *hardwired-procedures*
    '(do repeat call procedure-eval 
      return if cond in-environment 
      loop tell ;;;ftell
      unassert in-ontology) ;;; funassert)
  "The OCML procedures whose definition is 'hardwired'")


(defvar *ocml-eval-macro-symbol* '%%ocml-eval%%)
(defvar *ocml-list-of-macro-symbol* 'list-of)

;;;;;(defun ocml-eval-reader (stream subchar arg)
 ;;;; (declare (ignore subchar arg))
  ;;;;;(list *ocml-eval-macro-symbol*  (read stream t nil t)))

;;;;(defun ocml-list-of-reader (stream subchar arg)
 ;;; (declare (ignore subchar arg))
 ;;; (cons  *ocml-list-of-macro-symbol*  (read stream t nil t)))

;;;;(set-dispatch-macro-character #\# #\l #'ocml-list-of-reader)

;;;;(set-dispatch-macro-character #\# #\e #'ocml-eval-reader)

(defmacro with-default-eval-args-off (&body body)
  `(let ((*default-eval-args* nil))
     ,@body))

(defmacro with-default-eval-args-on (&body body)
  `(let ((*default-eval-args* t))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *in-irs* nil)

(defvar *base-ontology*)

(defparameter *base-ontology-name* 'base-ontology)

(defvar *all-ontologies* nil "All currently defined ontologies")
(defvar *current-ontology* nil 
  "The currently selected ontology")

(defvar *current-ontologies* nil 
  "All currently selected ontologies")

(defparameter *base-ontology-directory* 
  (string-append *library-pathname* "BASIC;"))

(defparameter *base-ontology-load-file* "load")

;(defparameter *base-ontology-files*
;    '("INFER"
;      "ENV"
;      "LIST2"
;      "FUNS"
;      "SETS"
;      "META"
;      "FRAMES2"
;      "NUMS"
;      "OPS2"
;      "RELS"
;      "STRINGS"
;      "TASK"))


;;;;;;;;;;;variables related to Mauro's compiler


(defvar *compile-trace* nil)   ;;; Show the code of compiled functions during compilation.
(defvar *compiled* nil)        ;;; Whe this variable is set to t the compiled code is executed.
(defvar *compiled-rules* nil)  ;;; Stores all the functions defined during the compilation of bc rules.
;;;;(defvar *compile* nil)         ;;; When this variable is set to t the lisp compiler is called.
(defvar *in-line* nil)         ;;; Whe this variable is set to t relations are compiled in line.


(defvar *nocompile* nil)      ;;;When T do not compile lisp code

(defvar *trace-depth-counter* 0)

(defvar *traced-tasks* nil)

(defvar *defined-functions* (make-hash-table) "All defined functions")

(defvar *inside-or-query* nil)

(defvar *depth* 0)

(defvar *spied-predicates* nil)

(defvar *domain-classes* (make-hash-table))

(defvar *axioms* (make-hash-table))

(defvar *defined-relations* (make-hash-table) "All defined relation types")

(defvar *bc-rules* (make-hash-table))

(defvar *namespace-prefixes* '()
  "Active namespace prefixes for reading OCML namespace sensitive symbols.")
