(in-package :ocml)

;;; {{{ Constants

;;; SBCL is anally-retentive about ‘redefinitions’ of defconstants, so
;;; we use this to placate it.
;;; http://www.sbcl.org/manual/Defining-Constants.html
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +ocml-version+ (asdf:component-version (asdf:find-system :ocml)))

(define-constant +ontology-types+
    '(applications domains goals mediators methods tasks web-services)
  "Classification schema for ontologies.")

(defvar *library-pathname* "ocml:library;")

(defvar *lisp-suffix* "lisp")

(defvar *ocml-top-class* 'ocml-thing)

(define-constant +legal-ocml-slot-options+
  '(:default-value :value :type  :inheritance :min-cardinality
    :max-cardinality :cardinality :documentation))

(define-constant +slot-info-options+
  '(:default-value :value :type :inheritance :min-cardinality
    :max-cardinality))

(define-constant +class-spec-lisp-options+
  '(:lisp-slots :lisp-class-name))

(define-constant +class-spec-slot-related-options+
  '(:own-slots :slot-renaming))

(define-constant +relation-spec-keywords+
  '(:constraint
    :exclusive-prove-by
    :iff-def
    :lisp-fun
    :no-proofs-by
    :prove-by
    :sufficient-for-type-checking
    :sufficient))

(define-constant +all-class-definition-legal-options+
  (append +class-spec-slot-related-options+
	  +class-spec-lisp-options+
          +relation-spec-keywords+
          '(:avoid-infinite-loop)))

(define-constant +value-options+
  '(:default-value :value))

(define-constant +non-value-options+
  '(:type :min-cardinality :max-cardinality))

(define-constant +inheritance-options+ '(:merge :supersede))

(define-constant +legal-clos-slot-options+
  '(:reader :writer :accessor :allocation :initarg :initform))

;;; }}}

(defvar *default-inheritance* :supersede)

(defvar *current-environment*)

(defvar *muffle-warnings* nil
  "Muffle warnings when defining ontologies.  Particularly aimed
  at redefinition warnings.")

(defvar *ignore-undefined-relations* :warn
  "When this variable is :ERROR, if we try to prove a goal (rel
   t1,...,tn) and rel has not been defined, an error is
   signalled.  If the value is :WARN, then a warning is issued.
   If the value is :IGNORE, then the undefined relation is
   ignored")

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

;;; XXX This has to go
(defvar *in-irs* nil)

(defvar *base-ontology*)

(defvar *base-ontology-name* 'base-ontology)

(defvar *base-ontology-directory* 
  (logical-pathname (string-append *library-pathname* "basic;")))

(defvar *base-ontology-load-file* "load")

(defvar *all-ontologies* nil "All currently defined ontologies")

(defvar *current-ontology* nil 
  "The currently selected ontology")

(defvar *current-ontologies* nil 
  "All currently selected ontologies")

;;; {{{ Mauro's compiler variables

(defvar *compile-trace* nil)   ;;; Show the code of compiled functions during compilation.
(defvar *compiled* nil)        ;;; Whe this variable is set to t the compiled code is executed.
(defvar *compiled-rules* nil)  ;;; Stores all the functions defined during the compilation of bc rules.
;;;;(defvar *compile* nil)         ;;; When this variable is set to t the lisp compiler is called.
(defvar *in-line* nil)         ;;; Whe this variable is set to t relations are compiled in line.

(defvar *nocompile* nil)      ;;;When T do not compile lisp code

;;; }}}

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

;;; {{{ Interface to OCML

(defgeneric translate (src dst ontology where &key &allow-other-keys)
  (:documentation "Translate ONTOLOGY from language SRC to DST."))

;;; }}}