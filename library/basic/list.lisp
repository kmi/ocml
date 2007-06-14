;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")


(in-ontology base-ontology)

(def-class list ()
   "A primitive class representing lists"
      ((element-type :type class))
      :lisp-fun  #'(lambda (x env)
                 (if (listp (instantiate x env)) ;;;make sure to instantiate x
                     (list env)
                     :fail)))

(def-class atom ()
   "A primitive class representing atoms"
      :lisp-fun  #'(lambda (x env)
                 (if (atom (instantiate x env)) ;;;make sure to instantiate x
                     (list env)
                     :fail)))

;;;List Manipulation and Access
(def-function list-of (&rest ?els)
   :lisp-fun #'(lambda (&rest els)
                 (apply #'list els)))


(def-function nth (?l ?n)
  :constraint (and (list ?l)(number ?n))
   :lisp-fun #'nth)

(def-function append (?l1 ?l2)
  :constraint (and (list ?l1)(list ?l2))
   :lisp-fun #'append)

(def-function position (?el ?l)
   :lisp-fun #'position)

(def-function length (?l)
  :constraint (list ?l)
   :lisp-fun #'length)

(def-function elt (?n ?l)
  :constraint (and (list ?l)(number ?n))
   :lisp-fun #'elt)

(def-function cons (?el ?l)
   :lisp-fun #'cons)

(def-function first (?l)
  :constraint (list ?l)
   :lisp-fun #'car)

(def-function second (?l)
  :constraint (list ?l)
   :lisp-fun #'cadr)

(def-function third (?l)
  :constraint (list ?l)
  :body (nth 2 ?l))

(def-function fourth (?l)
  :constraint (list ?l)
  :body (nth 3 ?l))

(def-function fifth (?l)
  :constraint (list ?l)
  :body (nth 4 ?l))


(def-function rest (?l)
  :constraint (list ?l)
   :lisp-fun #'cdr)

(def-function remove (?el ?l)
  :constraint (list ?l)
   :lisp-fun #'remove)

(def-function BUTLAST (?list)
  :body (cond ((null ?list) :nothing)
              ((null (rest ?list)) nil)
              ((true)
               (cons (first ?list) (butlast (rest ?list))))))

(def-function LAST (?list)
  :constraint (list ?list)
  :body (cond ((null ?list) :nothing)
              ((null (rest ?list)) (first ?list))
              ((true) (last (rest ?list)))))



;;;MAP
(def-function map (?fun ?l)
  :constraint (and (function ?fun)(list ?l))
  :body (if (null ?l)
          ?l
          (cons (call ?fun (first ?l))
                (map ?fun (rest ?l)))))



;;;MEMBER-FUN ---Define the relation Member
(defun member-fun (atom list env)
  (cond ((variable? list)
         (let ((val (lookup-or-self list env)))
           (if (variable? val)
               (List (bind val (list atom)))
               (ask-top-level `(member2 ,atom ,list) :all t :env env))))
        (t
         (ask-top-level `(member2 ,atom ,list) :all t :env env))))

;;;RELATION MEMBER
(def-relation member (?x ?y)
  :constraint (list ?y)
  :lisp-fun #'member-fun)


(def-rule member2
    ((member2 ?x (?x . ?y)))
    ((member2 ?x (?z . ?y))
       if
     (member2 ?x ?y)
    ))

;;;RELATION NULL
(def-relation null (?l)
   "True if ?l is bound to nil"
   :sufficient (= ?l nil))

