;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package ocml)


(eval-when (:compile-toplevel :load-toplevel)
  (proclaim '(inline bind binding-of)))

;;;BIND ---
(defun bind (var thing &optional env)
  (cons (cons var thing) env))

;;;BINDING-OF ---Returns 2 values: <flag, binding>. The flag says whether or not
;;;var is bound in env)
(defun binding-of (var env)
  (let ((pair (assoc var env :test #'eq)))
    (and pair
          (values t (cdr pair)))))



(defun variable? (pattern)
  (and (symbolp pattern)
       (char= (elt (symbol-name pattern)0)
              #\?)))


(defun match* (fact goal envs)
  (loop with result
        for env in envs
        for new-env = (match fact goal env)
        unless (eq new-env :fail)
        do
        (push new-env result)
        finally
        (return result)))

(defun match (fact goal &optional env)
  (multiple-value-bind (flag new-env)
      (simple-match fact goal env)
    (if flag
        new-env
        :fail)))



(defun simple-match (fact goal &optional env)
  (cond
   ((equal fact goal)
    (values t env))
   ((variable? goal)
    (match-var goal fact env))
   ((or (atom fact) (atom goal))
    nil)
   (t
    (multiple-value-bind (flag new-env) 
          (simple-match (car fact)(car goal) env)
      (when flag
	(simple-match (cdr fact)(cdr goal) new-env))))))



(defun match-var (var fact env)
  (multiple-value-bind (flag val)
      (lookup var env)
    (cond (flag
	   (if (variable? val)
               (values t
                       (bind val fact env))
               (simple-match fact val env)))
          (t  ;;;var is unbound
	   (values t
                   (bind var fact env))))))


;(defun match-var (var fact env)
;  (Let ((val (lookup-or-self var env)))
;    (if (variable? val)
;        (values t
;                (bind val fact env))
;        (simple-match fact val env))))



(defun instantiate (pattern environment) 
  (if environment
    (cond ((atom pattern)
           (if (variable? pattern)
             (multiple-value-bind (flag val)
		                  (lookup  pattern environment)
               (if flag
                 val
                 pattern))
             pattern))
          (t
           (cons (instantiate (car pattern) 
		              environment)
	         (instantiate (cdr pattern) 
		              environment))))
    pattern))
  

;(defun instantiate (pattern environment)
;  (cond ((null pattern)
;          nil)
;         ((atom pattern)
;	  (lookup-or-self pattern environment))
;         (t (cons (instantiate (car pattern) 
;		       environment)
;	      (instantiate (cdr pattern) 
;		       environment)))))

;;;RENAME-VARIABLES ---Substitutes the variables in <pattern> with newly-generated variable
;;;identifiers.  It returns the new pattern and the mapping between old and new vars.
(defun rename-variables (pattern &optional env)
  (cond ((variable? pattern)
         (multiple-value-bind (flag binding)
             (binding-of pattern env)
           (if flag
	    (values binding env)
	    (rename-var pattern env))))
        ((atom pattern)
         (values pattern env))
        (t
	 (multiple-value-bind (result new-env)
              (rename-variables (car pattern) env)
           (multiple-value-bind (result1 new-env1)
	       (rename-variables (cdr pattern) new-env)
             (values
              (cons result result1)
              new-env1))))))

(defun rename-var (var env)
  (let ((new-var (make-new-var var)))
    (values new-var (bind var new-var env))))

;;;MAKE-NEW-VAR ---Generates a new variable.  If the optional argument is supplied,
;;;it is essential that it is a symbol beginning with "?".
(defun make-new-var (&optional (symbol '?))
  (gensym (symbol-name symbol)))

(defun internal-var? (var)
  (not (find-symbol (symbol-name var))))

;;;RENAME-AND-SUBSTITUTE-VARS ----Takes as input a list of variables, <vars>, and an expression, <exp>.
;;;It replaces each occurrence in <exp> of a variable in <vars> with a newly generated variable.
;;;<vars> should not contain duplicates.
;;;For instance,
;;;   (rename-and-substitute-vars '(?x ?c ?v) '((?d ?f ?c (?x ?x) ?x p )?v))
;;; --->   ((?D ?F #:?C9541 (#:?X9540 #:?X9540) #:?X9540 P) #:?V9542)

(defun rename-and-substitute-vars (vars exp)
  (let ((var-alist (var-renaming-alist vars)))
    (substitute-vars var-alist exp)))

(defun var-renaming-alist (vars)
  (mapcar #'(lambda (var)
	      (cons var (make-new-var var)))
	  vars))

(defun substitute-vars (var-alist exp)
  (cond ((atom exp)
         (if (variable? exp)
             (or (right-value exp var-alist)
                 exp)
             exp))
        (t
         (cons (substitute-vars var-alist (car exp))
               (substitute-vars var-alist (cdr exp))))))

;;; RENAME-ARGS-AND-BODY --Renames the variables in schema and substitutes the new names
;;;to the old ones in  body.  It returns two values: the new schema, and the new body
(defun rename-args-and-body (schema body)
  (let ((alist (var-renaming-alist schema)))
    (values (mapcar #'cdr alist)
            (substitute-vars alist body))))

(defun rename-args-body-and-free-vars (schema body env)
  (let ((free-vars (filter (set-difference (collect-variables body)
                                   schema)
                           #'(lambda (var)
                               (not (assoc var env)))))
        (l (length schema)))
    (multiple-value-bind (vars new-body)
                         (rename-args-and-body (append schema free-vars) body)
      (values (subseq vars 0 l)
              new-body))))
    

;;;MULTIPLE-VARIABLE-BIND ---Binds a list of vars to a list of values.
;;;No checking is performed
;(defun multiple-variable-bind (vars values &optional env)
;  (if vars
;      (bind (car vars)(car values)
;             (multiple-variable-bind (cdr vars)(cdr values) env))
;      env))


;;;MULTIPLE-VARIABLE-BIND ---Binds a list of vars to a list of values.
;;;No checking is performed - Now handles &rest arguments
(defun multiple-variable-bind (vars values &optional env)
  (let ((l (length vars)))
    (cond ((> l 1)
           (let ((var (elt vars (- l 2))))
             (if (variable? var)
                 (multiple-variable-bind2 vars values env)
                 (bind (elt vars (- l 1))
                       (nthcdr (- l 2) values)
                       (multiple-variable-bind2 (butlast vars 2)
                                               (subseq values 0 (- l 2))
                                               env)))))
          (t
           (multiple-variable-bind2 vars values env)))))

(defun multiple-variable-bind2 (vars values &optional env)
  (if vars
      (bind (car vars)(car values)
             (multiple-variable-bind2 (cdr vars)(cdr values) env))
      env))


;;;UNIFY - goal1 goal2 &optional env)
;;;Top level unification function.  Returns :fail if no unification, the
;;;new environment otherwise
(defun unify (goal1 goal2 &optional env)
  (multiple-value-bind (flag new-env)
      (unify-r goal1 goal2 env)
    (if flag
        new-env
        :fail)))

;;;UNIFY-STRONG --This is the same as unify except that the structures of
;;;goal1 and goal2 should also match.  This means that either they are both atoms
;;;or they are lists with the same number of elements
;;;not match lists, only atoms
(defun unify-strong (goal1 goal2 &optional env)
  (setf goal1 (lookup-or-self  goal1 env)
        goal2 (lookup-or-self goal2 env))
  (cond ((and (atom goal1)(atom goal2))
         (unify goal1 goal2 env))
        ((and (consp goal1)(consp goal2))
         (let ((result (unify (car goal1)
                              (car goal2)
                              env)))
           (if (eq result :fail)
             :fail
             (unify (cdr goal1)
                    (cdr goal2)
                    result))))
        (t
         :fail)))
  
   
(defun unify-r (pattern-a pattern-b &optional env)
  (cond
    ((equal pattern-a pattern-b)               ;;If they are equal, they match.
     (values t env))
    ((variable? pattern-a)                     ;;pattern is a variable.
     (unify-var
      pattern-a pattern-b env))
    ((variable? pattern-b)
     (unify-var
      pattern-b pattern-a env))
    ((and (numberp pattern-a)(numberp pattern-b))
     (values (= pattern-a pattern-b)
             env))
    ((or (atom pattern-a)(atom pattern-b))    ;;If one arg is an atom we fail
     nil)
    (t
     (multiple-value-bind (flag new-env)
         (unify-r (car pattern-a)(car pattern-b) env)
       (when flag
	 (unify-r (cdr pattern-a)
                  (cdr pattern-b)
                  new-env))))))

  

(defun unify-var (var pattern env)
  (multiple-value-bind (flag-1 binding-1)
      (lookup var env)
    (if flag-1 ;;;var is bound
        (if (variable? pattern)
          ;;var is bound and pattern is a var
            (multiple-value-bind (flag-2 binding-2)
	        (lookup pattern env)
              
	      (if flag-2 
                ;;both var and pattern are bound
                (unify-r binding-1 binding-2 env) 
                ;;var is bound and pattern is not
                ;;let's match pattern to binding-1
                ;;unless pattern = binding-1
                (if (equal pattern binding-1)
                  (values t env)
                  (values t
                          (bind pattern  binding-1 ;;;;var
                                env)))))
            (unify-r                               ;;pattern is not a variable
             binding-1 pattern env ))              ;;and var is bound
        
        ;;var is unbound
	(if (variable? pattern)
            (multiple-value-bind (flag-2 binding-2)
	        (lookup pattern env)
              (if flag-2  
                ;;pattern is bound, var is unbound
                 ;;let's match var to binding-2
                ;;unless var = binding-2
                (if (equal var binding-2)
                  (values t env)
                  (values t
                          (bind var  binding-2
                                env)))
                ;;both pattern and var are unbound
                (values t 
                        (if (internal-var? pattern)
                          (bind  pattern var env)
                          (bind var pattern env)))))
            
            ;;var is unbound, pattern is not a variable
            (values t
		    (bind var pattern env))))))
             
;(defun unify-var (var pattern env)
;  (multiple-value-bind (flag-1 binding-1)
;      (lookup var env)
;    (if flag-1 ;;;var is bound
;        (if (variable? pattern)
;            (multiple-value-bind (flag-2 binding-2)
;	        (lookup pattern env)
;	      (if flag-2 ;;;pattern is bound
;	          (unify-r binding-1 binding-2 env) 
;	          (values t
;                          (bind pattern var
;                                env))))
;            (unify-r                               ;;pattern is not a variable
;             binding-1 pattern env ))              ;;and var is bound
;	(values t
;                (bind var pattern env)))))



;;;LOOKUP-OR-SELF --Returns the ultimate binding of <thing> in <env>,
;;;if <thing> is bound, or <thing> itself, if it is not.
(defun lookup-or-self (thing env)
  (if (variable? thing)
    (multiple-value-bind (flag result)
                         (lookup thing env)
      (if flag
        result
        thing))
    (if (atom thing)
      thing
      (cons (lookup-or-self (car thing) env)
            (lookup-or-self (cdr thing) env)))))

(defun unbound-variable? (var env)
  "This function returns NIL either if var is bound to something 
   which is not a variable, or if var IS NOT a variable. 
   Otherwise, if var is not bound to anything, 
   then var is returned, otherwise the variable to which var is bound
   is returned.

   For instance

   ? (UNBOUND-VARIABLE? '?c '((?c . 2)))  -> NIL
   ? (UNBOUND-VARIABLE? '?c '((?c . ?x)(?x . ?y))) -> ?Y
   ? (UNBOUND-VARIABLE? '?c '((?ee . ?x))) -> ?C

              "
  (when (variable? var)
    (let ((result (lookup-or-self var env)))
      (if (eq var result)
        var
        (if (variable? result)
          result)))))
      
            

;(defun lookup-or-self (thing env)
;  (multiple-value-bind (flag result)
;                       (lookup thing env)
;    (if flag
;      result
;      thing)))

;;;LOOKUP ---This function can be used to retrieve the 
;;;'ultimate' binding of a variable in an environment. 
;;;This can be an unbound variable.
;;;LOOKUP returns two values, <flag, binding>
(defun lookup (var env)
  (multiple-value-bind (flag binding)
                       (binding-of var env)
    (when flag
      (cond ((variable? binding)
             (multiple-value-bind (flag binding-2)
                                  (lookup binding env)
               (if flag
                 (values t binding-2)
                 (values t binding))))
            ((ground-termp binding)
             (values t binding))
            (t ;;;it must be a list
             (values t 
                     (cons (lookup-or-self (car binding) env)
                           (lookup-or-self (cdr binding) env))))))))

      
;(defun lookup (thing env &optional (result nil supplied-p))
;  (cond ((variable? thing)
;         (multiple-value-bind (flag binding)
;             (lookup-var thing env)
;           (if flag
;               (values t binding)
;               (when supplied-p
;		 (values t result)))))
;        ((atom thing)
;         (values t thing))
;        (t
;	 (values t
;	         (cons (lookup-or-self (car thing) env)
;                       (lookup-or-self (cdr thing) env))))))


;;;LOOKUP-VAR  ---Returns the ultimate binding of a var in an environmenmt.  The result is not
;;;necessarily a ground term
;(defun lookup-var (var env)
;  (multiple-value-bind (flag binding)
;      (binding-of var env)
;    (when flag
;        (if (ground-termp binding)
;	    (values t binding)
;            (multiple-value-bind (flag2 binding2) 
;                                 (lookup binding 
;                    ;;;Change to avoid infinite loop in recursive envs ---15/10/96
;                                         (remove (cons var binding) env :test #'equal)
;                                         binding)
;              (if flag2
;                (if (eq binding2 var)
;                  (values t binding)
;                  (values t binding2))
;                (values t binding)))))))
                
                  




;(defun variable? (thing)
;  (eri::variable? thing))

;(defun unbound? (var env)
;  (variable? (lookup-or-self var env)))

(defun ground-termp (thing)
  (if (atom thing)
      (not (variable? thing))
      (and (ground-termp (car thing))
           (ground-termp (cdr thing)))))


(defun collect-variables (l &optional vars)
  (cond ((consp l)
         (collect-variables (cdr l)
                            (collect-variables (car l) vars)))
        ((variable? l)
         (pushnew l vars :test #'eq))
        (t
         vars)))
                                   
         
;            
;
;
;(defun collect-variables (l)
;  (remove-duplicates (collect-variables-recursively l)))
;
;(defun collect-variables-recursively (l)
;  (when l
;    (if (atom l)
;        (when (variable? l)
;          (list l))
;        (append (collect-variables-recursively (car l))
;                (collect-variables-recursively (cdr l))))))