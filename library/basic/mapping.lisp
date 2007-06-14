;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package ocml)

(in-ontology base-ontology)


;;;Here I define the library support for a simple mechanism for 
;;;defining mappings between objects in different models.  For instance
;;;in the sisyphus1 room application we defined a mapping between
;;;the concepts in the task ontology (e.g. parameters) and the 
;;;members of teh YQT lab.  As shown here, we use the macro
;;;def-upward-class-mapping to create the yqt parameters
;;;corresponding to yqt members and to associate them by means
;;;of the relation MAPS-TO.  There is nothing special with this relation
;;;It is just a convention and any other name could be used.

;;;(def-upward-class-mapping yqt-member yqt-parameter)

;;;having linked yqt members to yqt parameters, I define
;;;the sis1 design model and specify the relation 
;;;yqt-parameter-value as the membership test. This relation
;;;associates a yqt parameter to its value in the sis1 design model.


;;;(def-instance sis1-design-model design-model
;;;  ((membership-test yqt-parameter-value)))
;;;
;;;
;;;(def-relation yqt-parameter-value (?param ?room)
;;; :constraint (and (yqt-parameter ?param)
;;;                   (room ?room)))
;;;

;;;Finally, I define the mappings between YQT-PARAMETER-VALUE
;;;and the domain relation IN-ROOM
;;;
;;;(def-relation-mapping yqt-parameter-value :up
;;;   ((yqt-parameter-value ?x ?v)
;;;    if
;;;    (maps-to ?X ?Z)
;;;    (in-room ?z ?v)))
;;;
;;;
;;;(def-relation-mapping yqt-parameter-value (:down :add)
;;;   (lambda (?x ?v)
;;;     (if (maps-to ?X ?Z)
;;;       (tell (in-room ?z ?v)))))
;;;
;;;
;;;
;;;(def-relation-mapping yqt-parameter-value (:down :remove)
;;;   (lambda (?x ?v)
;;;     (if (maps-to ?X ?Z)
;;;       (unassert (in-room ?z ?v)))))
;;;


;;;In this file we define a simple ontological support to perform 
;;;mappings

(def-relation maps-to (?x ?y)
  "This relation allows the user to specify an association between
   an object at the task layer and one at the domain layer.
   Formally ?y is the object denoted by ?x"
  :iff-def (= ?y (denotation ?x))
  :no-proofs-by (:iff-def))


(def-function domain-reference (?x) -> ?y
  :body (the ?y (maps-to ?x ?y)))

(def-function meta-reference (?y)
   :body (the ?x (maps-to ?x ?y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun domain-reference (x)
  (ocml-eval-fun `(domain-reference ',x)))

(defun meta-reference (x)
  (ocml-eval-fun `(meta-reference ',x)))

