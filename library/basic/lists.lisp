;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")


(in-ontology base-ontology)


(def-class pair (list)?x
   "A  class representing a list with two elements."
   :iff-def
   (and (list ?x) (= (length ?x) 2))
   :lisp-fun #'(lambda (x env)
                 (let ((instantiated-x (instantiate x env)))
                 (if (and (listp instantiated-x)
                          (= (length instantiated-x) 2))
                   (list env)
                   :fail))))

(def-class atom () ?x
   "A  class representing atoms"
   :lisp-fun  #'(lambda (x env)
                (if (atom (instantiate x env)) ;;;make sure to instantiate x
                     (list env)
                     :fail)))
;;;CLASS NULL
(def-class null (list) ?l
   "True if ?l is bound to nil"
   :iff-def (= ?l nil))


;;;INSTANCE NIL
(def-instance nil null
   "NIL is the (only) empty list")


;;;;;;;;;;;;;;;;;;;List Manipulation and Access;;;;;;;;;;


;;;FUNCTION LIST-OF
(def-function list-of (&rest ?els)
   "This is the primitive list constructor.  It is implemented in terms of
    the underlying LISP list construction primitive, LIST"
   :lisp-fun #'(lambda (&rest els)
                 (apply #'list els)))




;;;FUNCTION LENGTH ---Returns the length of a list
(def-function length (?l)
  :constraint (list ?l)
   :lisp-fun #'length)

;;;FUNCTION  ELT---Returns the nth element of a list, or :nothing, if ?n is
;;;greater or equal to the length of the list. As in lisp, positions start at index
;;;0.
(def-function elt (?n ?l)
  :constraint (and (list ?l)(number ?n)(< ?n (length ?l)))
  :body (in-environment ((?length . (length ?l)))
                        (if (< ?n ?length)
                            (if (= ?n 0)
                                (first ?l)
                                (elt (- ?n 1)(rest ?l)))
                            :nothing)))


;;;FUNCTION APPEND --Appends two lists
(def-function append (?l1 &rest ?ls)
  :constraint (and (list ?l1)(every  ?ls list))
   :lisp-fun #'append)


;;;FUNCTION POSITION --
(def-function position (?el ?l)
   "Returns the index corresponding to the position of an item in the list,
    or :nothing if ?el is not in ?l"
   :constraint (list ?l)
   :body (if (null ?l)
             :nothing
             (if (= (first ?l) ?el)
                 0
                 (in-environment ((?x . (position ?el (rest ?l))))
                     (if (= ?x :nothing)
                         :nothing
                         (+ 1 ?x))))))

;;;RELATION PRECEDES
(def-relation precedes (?item1 ?item2 ?list)
  :iff-def (and (member ?item1 ?list)
                (member ?item2 ?list)
                (= ?n1 (position ?item1 ?list))
                (= ?n2 (position ?item2 ?list))
                (< ?n1 ?n2)))

;;;FUNCTION CONS
(def-function cons (?el ?l)
   :constraint (list ?l)
   :lisp-fun #'cons)


;;;FUNCTION FIRST
(def-function first (?l)
   "Takes the first element of a list.  If the list is empty
    the function returns :nothing"
  :constraint (list ?l)
  :body (if (= ?l (?a . ?b))
            ?a
            :nothing))

;;;FUNCTION SECOND
(def-function second (?l)
   "Takes the second element of a list.  If the list is empty
    the function returns :nothing"
  :constraint (list ?l)
  :body (if (= ?l (?a . ?b))
            (first ?b)
            :nothing))

;;;FUNCTION THIRD
(def-function third (?l)
   "Takes the third element of a list.  If the list is empty
    the function returns :nothing"
  :constraint (list ?l)
  :body (elt 2 ?l))

;;;FUNCTION FOURTH
(def-function fourth (?l)
   "Takes the fourth element of a list.  If the list is empty
    the function returns :nothing"
  :constraint (list ?l)
  :body (elt 3 ?l))

;;;FUNCTION FIFTH
(def-function fifth (?l)
   "Takes the fifth element of a list.  If the list is empty
    the function returns :nothing"
  :constraint (list ?l)
  :body (elt 4 ?l))


;;;FUNCTION REST
(def-function rest (?l)
   "Returns the elements of a list but the first one.  If the list
    is empty, then NIL is returned"
  :constraint (list ?l)
  :body (if (= ?l (?a . ?b))
            ?b
            nil))


;;;FUNCTION REMOVE
(def-function remove (?el ?l)
   "Removes all occurrences of ?el in ?l"
   :constraint (list ?l)
   :lisp-fun #'remove)


(def-function remove-duplicates (?l)
  :constraint (list ?l1)
   :lisp-fun #'remove-duplicates)

;;;FUNCTION BUTLAST
(def-function BUTLAST (?list)
   "Returns all the element of ?list, except the last one.
    If ?list = NIL, then :nothing is returned.  If ?list has
    length 1, then nil is returned"
   :constraint (list ?l)
   :body (cond ((null ?list) :nothing)
              ((null (rest ?list)) nil)
              ((true)
               (cons (first ?list) (butlast (rest ?list))))))


;;;FUNCTION LAST
(def-function LAST (?list)
   "Returns the last element of a list.  If ?list is empty
    then :nothing is returned"
  :constraint (list ?list)
  :body (cond ((null ?list) :nothing)
              ((null (rest ?list)) (first ?list))
              ((true) (last (rest ?list)))))


(def-function SUBLIST (?l ?length)
  :body (if (<= ?length (length ?l))
          (if (= ?length 0)
            nil
            (cons (first ?l)
                  (sublist (rest ?l) (-  ?length 1))))))

;;;FUNCTION NTHREST
(def-function nthrest (?l ?n)
   "Takes a list, ?l, and a positive integer, ?n, and returns
    the list obtained by removing the first ?n element from ?l"
   :constraint (and (List ?l)
                    (number ?n)
                    (> ?n 0))
   :body (if (= ?n 0)
             ?l
             (nthrest (rest ?l)
                      (- ?n 1))))


;;;FUNCTION MAP
(def-function map (?fun &rest ?ls)
  :constraint (and (function ?fun)(every ?ls list)) 
  :body (if (null (first ?ls))
            nil
            (cons (apply ?fun (map1 first ?ls))
                  (apply map (cons ?fun (map1 rest ?ls))))))

(def-function map1 (?fun ?l)
  :constraint (and (function ?fun)(list ?l))
  :body (if (null ?l)
            ?l
            (cons (call ?fun (first ?l))
                  (map1 ?fun (rest ?l)))))

;(def-function map (?fun ?l)
;  :constraint (and (function ?fun)(list ?l))
;  :body (if (null ?l)
;            ?l
;            (cons (call ?fun (first ?l))
;                  (map ?fun (rest ?l)))))
;;;FUNCTION FILTER
(def-function filter (?l ?rel)
  "Returns all the elements in ?l which satisfy ?rel"
  :constraint (and (unary-relation ?rel)
                   (list ?l))
  :body (if (null ?l)
          ?l
          (if (holds ?rel (first ?l)) ;;new holds format
            (cons (first ?l)
                  (filter (rest ?l) ?rel))
            (filter (rest ?l) ?rel))))

;;;REVERSE
(def-function reverse (?list)
  :lisp-fun #'reverse)



(def-relation EVERY (?l ?rel)
  "True if for each term in ?l, say ?term, (holds ?rel ?term) is true"
  :constraint (unary-relation ?rel)
  )

(def-rule every
  ((every nil ?rel))
  ((every (?head . ?tail) ?rel)
   if
   (holds ?rel ?head) ;;;new holds format
   (every ?tail ?rel)))


(def-relation SOME (?l ?rel)
  "True if for SOME term in ?l, say ?term, (holds ?rel ?term) is true"
  :constraint (unary-relation ?rel)
  )

(def-rule some
  ((some nil ?rel)
   if
   (fail))
  ((some (?head . ?tail) ?rel)
   if
   (holds ?rel ?head))
  ((some (?head . ?tail) ?rel)
   if
   (some  ?tail ?rel)))


;;;RELATION MEMBER
(def-relation member (?x ?y)
  :constraint (list ?y))


(def-rule member
    ((member ?x (?x . ?y)))
    ((member ?x (?z . ?y))
       if
     (member ?x ?y)))

(def-function right-value (?x ?l)
  :body (The ?v (some ?l (kappa (?pair)
                                (== ?pair (?x . ?v))))))

(def-function left-value (?x ?l)
  :body (The ?v (some ?l (kappa (?pair)
                                (== ?pair (?v . ?x))))))


