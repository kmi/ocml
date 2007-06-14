;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

;;;;(def-function select-next-in-role (?role-as-list ?role-as-token)
;;;;   :body (if (role-value-exists-p ?role-as-token)
;;;;             (select-next (role-value ?role-as-list)
;;;;                          (role-value ?role-as-token))
;;;;             (first (role-value ?role-as-list))))

;;;FUNCTION SELECT-NEXT
(def-function select-next (?list ?item)
  :constraint (list ?list)
  :body (if (member ?item ?list)
               (if (and (= ?pos (position ?item ?list))
                        (< (+ 1 ?pos) (length ?list)))
                 (elt (+ 1 ?pos) ?list))
               (first ?item)))

;;;FUNCTION SELECT-ANY
(def-function select-any (?list)
  :constraint (list ?list)
  :body (the ?x (member ?x ?list)))


;;;FUNCTION SORT
(def-function sort (?list ?r)
  :constraint (and (list ?list)(relation ?r))
  :lisp-fun #'(lambda (l r)
                (sort l #'(lambda (x y)
                            (holds-in-env? r (List x y)
                                           *current-environment*)))))

                            ;;;(goal-holds? (list r x y))))))



