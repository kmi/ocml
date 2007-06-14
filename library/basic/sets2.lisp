;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")


(in-ontology base-ontology)




(def-class SET ()
  ((membership-test :type relation)))

(def-class INDIVIDUAL () ?x
  "An individual is something which is not a set.
    Therefore lists which represent sets are not
    individuals"
  :iff-def (not (set ?x)))

(def-rule relation-membership-test
  ((membership-test ?rel ?rel)
   if
   (relation ?rel)))
   
(def-class SET-AS-LIST (set) ?l
   "True of a list which does not contain duplicates"
   :iff-def (or (null ?l)
                (and (List ?l)
                     (not (null ?l))
                     (not (member (first ?l)(rest ?l)))
                     (set-as-list (rest ?l)))))

		   ;;  (not (exists ?el
                   ;;               (and (member ?el ?l) ;;This clause is needed to ensure
                   ;;                                ;;?el is bound when calling
                   ;;                                ;;position
                   ;;                    (= ?i (position ?el ?l))
                   ;;                    (member ?el
                   ;;                            (nthrest ?l (+ 1 ?i)))))))))

(def-function CARDINALITY (?set)
  "This is defined on sets and relations. The cardinality of 
   a set is the number of elements in the set.  The cardinality
   of a relation is the number of element in its extension"
  :constraint (or (set-as-list ?set)(relation ?set))
  :body (if (set-as-list ?set)
          (length ?set)
          (if (relation ?set)
            (length (extension ?set)))))

                                       
;;;FUNCTION SET-OF
(def-function set-of (&rest ?els)
  "A set constructor"
   :body (apply list-of ?els))


;;;ELEMENT-OF
(def-relation element-of (?x ?set)
   :iff-def (or (and (set-as-list ?set)(member ?x ?set))
                (and (not (set-as-list ?set))
                     (exists ?rel (membership-test ?set ?rel))
                     (or (and (unary-relation ?rel)
                              (holds ?rel ?x))
                         (and (not (unary-relation ?rel))
                              (= ?X (rename-variables (the-schema ?rel)))
                              (goal-holds (cons ?rel ?x)))))))


;;;EMPTY-SET
(def-class EMPTY-SET (set) ?set
   :iff-def (not (exists ?x (element-of ?x ?set))))

;;;UNION
(def-function union (&rest ?sets)
    :constraint (every ?sets set)
    :body (setofall ?x
                   (exists ?set (and (member ?set ?sets)
                                     (element-of ?x ?set)))))
;;;INTERSECTION
(def-function intersection (&rest ?sets)
  :constraint (every ?sets set)
  :body (in-environment ((?all . (apply union ?sets)))
             (setofall ?x
                   (and (element-of ?x ?all)
                        (not (exists ?set
                                     (and (member ?set ?sets)
                                          (not (element-of ?x ?set)))))))))

;;;DIFFERENCE
(def-function DIFFERENCE (?set &rest ?sets)
  :constraint (and (set ?set)(every ?sets set))
  :body (setofall ?x
                  (and (element-of ?x ?set)
                       (not (exists ?s
                                    (and (member ?s ?sets)
                                         (element-of ?x ?s)))))))

(def-function DIFFERENCE_2 (?set1 ?set2)
  :constraint (and (set-as-list ?set1)(set-as-list ?set2))
  :lisp-fun #'set-difference)


(def-relation SAME-SET (?s1 ?s2)
  :iff-def (and (subset ?s1 ?s2)(subset ?s2 ?s1))
  :prove-by (and (= (difference_2 ?s1 ?s2) nil)
                 (= (difference_2 ?s2 ?s1) nil))
  :no-proofs-by (:iff-def))
                 

;;;SUBSET
(def-relation SUBSET (?s1 ?s2)
   :constraint (and (set ?s1)(set ?s2))
   :iff-def (not (exists ?x
                         (and (element-of ?x ?s1)
                              (not (element-of ?x ?s2))))))

(def-relation DISJOINT (?s1 ?s2)
  "Two sets are disjoint if and only if there is no object that is a
   member of both sets."
  :iff-def (empty-set (intersection ?s1 ?s2)))


(def-relation PAIRWISE-DISJOINT (&rest ?sets)
  "Sets are pairwise-disjoint if and only if every
   set is disjoint from every other set."
  :iff-def (not (exists (?s1 ?s2)
                   (and (member ?s1 ?sets)
                        (member ?s2 ?sets)
                        (not (= ?s1 ?s2))
                        (not (disjoint ?s1 ?s2))))))


(def-relation MUTUALLY-DISJOINT (&rest ?sets)
  "Sets are mutually-disjoint if and only if there is no object that
   is a member of all of the sets."
  :iff-def (empty-set (apply intersection ?sets)))


(def-relation SET-PARTITION (?s &rest ?sets)
  "True if ?sets is a partition of ?s"
  :constraint (and (set ?s)(every ?sets set))
  :iff-def (and (= ?s (apply union ?sets))
                (goal-holds (cons pairwise-disjoint ?sets))))
