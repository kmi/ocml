;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-


;;; File: New Support for Backward Chaining in OCML
;;; Author: Mauro Gaspari
;;; Date: August 1995


(in-package "OCML")

;;;ASK-TOP-LEVEL
(defun ask-top-level (goal &optional all? env query-mode? compiled)
  (unless (listp goal)
    (error "A goal must be a list..when parsing ~s" goal))
  (ask-internal goal all? env query-mode? t))


(defun ask-internal (query &optional all? env query-mode? compiled)
  (let ((pred (car query)))
    (when (variable? pred)
      (multiple-value-bind (flag binding)
          (lookup-var pred env)
        (if flag
            (Setf pred binding)
            (error "Can't handle query ~S in env. ~s" query env))))
    (case pred
      (not
       (prove (list query):all all? :env env :query-mode? query-mode? :compiled
	      compiled
              ))
       ;;;;;(ask-not (second query)))
      (and
       (prove (cdr query) :all all? :env env :query-mode? query-mode? :compiled
	      compiled
              ))
      (or (ask-or (cdr query)env query-mode? compiled  ))
      (exists (ask-exists-query (second query)(third query)env query-mode? 
	      compiled))
      (otherwise
       (prove (list query) :all all? :env env :query-mode? query-mode? :compiled
	      compiled
             )))))


;;;ASK-OR ---Succeeds if one of the goals in <l> is satisfied
(defun ask-or (l &optional env query-mode? compiled)
  (if l
      (let ((result (ask-internal (car l) nil env query-mode? compiled)))
        (if (eq result :fail)
            (if (cdr l)
                (ask-or (cdr l)env query-mode? )
                :fail)
            result))
      l))

(defun ask-exists-query (vars query &optional env query-mode? compiled)
  (let ((env (ask-internal query nil env nil compiled)))
    (unless (Listp vars)
      (setf vars (list vars)))
    (if (eq env :fail)
        (if query-mode?
            (format t "No (more) solutions")
            :fail)
	(Let ((new-env
	       (mapcar #'(lambda (var)
                           (multiple-value-bind (flag binding)
			       (lookup var env)
                             (declare (ignore flag))
		             (cons var binding)))
                       vars)))
          (if query-mode?
              (format t "~2%Solution: ~{~S ~}"new-env)
	      new-env)))))

(defun prove (goals &key all env query-mode? compiled &aux bindings)
  (if compiled
      (setf bindings
	    (eval `(call-bc-rule ,goals ,all ,env ,query-mode? ;;;;;nil 
                                 nil)))
    (setf bindings (*prove goals :all all :env env :query query-mode? )))
  (unless query-mode?
      (if bindings
	  (if all
              bindings
              (car bindings))
	:fail)))



(defun reset-sis1-demo ()
  (unassert (in-room ?C ?X))
  (set-role-value 'parameter-types
                  (ocml-eval (setofall ?x (yqt-member-type ?x)))))

(defun munb ()
  (maphash #'(lambda (k x)
               (with-slots (lisp-fun) x
                 (when lisp-fun 
                   (setf lisp-fun nil))))
	   *defined-relations*)
  (maphash #'(lambda (k x)
               (with-slots (lisp-fun) x
                 (when lisp-fun 
                   (setf lisp-fun nil))))
	   *defined-functions*))
  

  

