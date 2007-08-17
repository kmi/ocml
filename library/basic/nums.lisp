;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

(def-class NUMBER ()
   "The class of all numbers"
   :lisp-fun  #'(lambda (x env)
                 (if (numberp (instantiate x env)) ;;;make sure to instantiate x
                     (list env)
                     :fail)))

(def-class VECTOR () ?x
   "A vector is a list of numbers"
   :iff-def (and (list ?x)
                 (every ?x number)))




(def-class INTEGER (number)
   "The class of all integers"
   :lisp-fun  #'(lambda (x env)
                 (if (integerp (instantiate x env)) ;;;make sure to instantiate x
                     (list env)
                     :fail)))

(def-class float (number)
  "Floating point numbers."
  :lisp-fun
  #'(lambda (x env)
      (if (floatp (instantiate x env)) ;; Ensure x is instantiated.
	  (list env)
	  :fail)))

(def-class NON-NEGATIVE-INTEGER (integer)
   "The class of all integers >= 0"
   :lisp-fun  #'(lambda (x env)
                  (let ((y (instantiate x env)))
                 (if (and (integerp y)
                          (>= y 0));;;make sure to instantiate x
                     (list env)
                     :fail))))

(def-class POSITIVE-INTEGER (integer)
   "The class of all integers > 0"
   :lisp-fun  #'(lambda (x env)
                  (let ((y (instantiate x env)))
                 (if (and (integerp y)
                          (> y 0));;;make sure to instantiate x
                     (list env)
                     :fail))))

(def-class REAL-NUMBER (number)
   "The class of all numbers"
   :lisp-fun  #'(lambda (x env)
                 (if (realp (instantiate x env)) ;;;make sure to instantiate x
                     (list env)
                     :fail)))

(def-class positive-REAL-NUMBER (real-number) ?x
           "The class of all positive real numbers"
           :iff-def (and (real-number ?x)
                         (> ?x 0))
           :lisp-fun   #'(lambda (x env)
                           (let ((y (instantiate x env)))
                             (if (and (realp y)
                                      (> y 0));;;make sure to instantiate x
                                 (list env)
                                 :fail))))

(def-function the-floating-point-number (?X)
  :lisp-fun #'(lambda (x)
                (coerce x 'float)))

(def-function + (?X &rest ?y)
  "Adds numbers"
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'+)

(def-function * (?X ?y)
  :constraint (and (number ?x) (number ?y))
   :lisp-fun #'*)

(def-function square (?X)
  :constraint  (number ?x)  
  :body (* ?X ?X))

(def-function - (?X &rest ?y)
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'(lambda (x &rest y)
                (apply #'- x y)))

(def-function / (?X ?y)
  :constraint (and (number ?x) (number ?y))
  :lisp-fun #'/)

(def-function floor (?X ?y)
   :lisp-fun #'floor)

(def-function asin (?X ?y)
   :lisp-fun #'asin)

(def-function sqrt (?X ?y)
   :lisp-fun #'sqrt)

(def-function pi ()
   :lisp-fun #'(lambda ()pi))

(def-function min (?numbers)
  :constraint (every ?numbers number)
  :lisp-fun #'(lambda (numbers) (apply #'min numbers)))

(def-function max (?numbers)
  :constraint (every ?numbers number)
  :lisp-fun #'(lambda (numbers) (apply #'max numbers)))


                 
;;; RELATION <
(def-relation < (?x ?y)
   "A predicate to test whether a number is less than another"
   :constraint (and (number ?x)(number ?y))
   :lisp-fun #'(lambda (x y env)
                 (if (< (instantiate x env)
                        (instantiate y env))
		     (List env)
		     :fail)))

(def-relation > (?x ?y)
   "A predicate to test whether a number is greater than another"
   :constraint (and (number ?x)(number ?y))
   :lisp-fun #'(lambda (x y env)
                 (if (> (instantiate x env)
                        (instantiate y env))
		     (List env)
		     :fail)))

(def-relation <= (?x ?y)
   "A predicate to test whether a number is less or equal to another"
   :constraint (and (number ?x)(number ?y))
   :lisp-fun #'(lambda (x y env)
                 (if (<= (instantiate x env)
                        (instantiate y env))
		     (List env)
		     :fail)))

(def-relation >= (?x ?y)
   "A predicate to test whether a number is greater or equal to another"
   :constraint (and (number ?x)(number ?y))
   :lisp-fun #'(lambda (x y env)
                 (if (>= (instantiate x env)
                        (instantiate y env))
		     (List env)
		     :fail)))
