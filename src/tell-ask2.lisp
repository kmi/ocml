;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package ocml)

(defun and-expression? (exp)
  (and (listp exp)
       (eq (car exp )'and)))

(defun ask-internal (query &optional all? env query-mode? )
  (let ((pred (car query)))
    (when (variable? pred)
      (multiple-value-bind (flag binding)
          (lookup-var pred env)
        (if flag
            (Setf pred binding)
            (error "Can't handle query ~S in env. ~s" query env))))
    (case pred
      (not
       (prove (list query):all all? :env env :query-mode? query-mode?
              ))
       ;;;;;(ask-not (second query)))
      (and
       (prove (cdr query) :all all? :env env :query-mode? query-mode?
              ))
      (or (ask-or (cdr query)env query-mode?  ))
      (exists (ask-exists-query (second query)(third query)env query-mode? ))
      (otherwise
       (prove (list query) :all all? :env env :query-mode? query-mode?
             )))))


;;;PROVE ---Proves a list of goals. It returns
;;;Nil if query-mode? is t
;;;One env if all is nil
;;;A list of envs if all is t
(defun prove (goals &key all env query-mode? )
  (let ((bindings (*prove goals :all all :env env :query query-mode? )))
    (unless query-mode?
      (if bindings
	  (if all
              bindings
              (car bindings))
          :fail))))



;;;ASK-OR ---Succeeds if one of the goals in <l> is satisfied
(defun ask-or (l &optional env query-mode? )
  (if l
      (let ((result (ask-internal (car l) nil env query-mode? )))
        (if (eq result :fail)
            (if (cdr l)
                (ask-or (cdr l)env query-mode? )
                :fail)
            result))
      l))

(defun ask-exists-query (vars query &optional env query-mode? )
  (let ((env (ask-internal query nil env)))
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


        
          
