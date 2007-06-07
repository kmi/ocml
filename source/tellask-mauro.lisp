;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package ocml)

(defvar *inside-or-query* nil)

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
       (setf query (maybe-canonicalize-not query))
       (prove (list query):all all? :env env :query-mode? query-mode?
              ))
      (and
       (prove (cdr query) :all all? :env env :query-mode? query-mode?
              ))
      (or (ask-or (cdr query)all? env query-mode?  ))
      (exists (ask-exists-query (second query)(third query)env query-mode? ))
      (otherwise
       (prove (list query) :all all? :env env :query-mode? query-mode?
             )))))

(defun maybe-canonicalize-not (body)
  (if (eq (car (second body)) 'or)
      `(and 
      ,@(mapcar #'(lambda (exp)
                    (if (eq (car exp) 'not)
                        (second exp)
                        (maybe-canonicalize-not (list 'not exp))))
              (cdr (second body))))
      body))
      


;;;PROVE ---Proves a list of goals. It returns
;;;Nil if query-mode? is t
;;;One env if all is nil
;;;A list of envs if all is t
(defun prove (goals &key all env query-mode? )
  (let ((bindings (*prove goals :all all :env env :query query-mode? )))
    (if query-mode?
        bindings
      (if bindings
	  (if all
              bindings
              (car bindings))
          :fail))))

(defun prove-template-int (goals &key all env query-mode?)
  (let ((bindings (*prove goals :all all :env env :query query-mode? )))
    (if query-mode?
        bindings
      (if bindings
	  (if all
              bindings
              (car bindings))
          :fail))))

(defun prove-template-cmp (goals &key all env query-mode?)
  (let ((bindings (eval `(call-bc-rule ,goals ,all ,env ,query-mode? nil nil))))
    (if query-mode?
        bindings
      (if bindings
	  (if all
              bindings
              (car bindings))
          :fail))))


;;;ASK-OR ---Succeeds if one of the goals in <l> is satisfied
(defun ask-or (l &optional all? env query-mode? )
  (if l
      (let* ((*inside-or-query* t)
	     (result (ask-internal (car l) all?  env query-mode? )))
        (cond (query-mode?
	       (unless (eq result :quit)
		 (if (cdr l)
		     (ask-or (cdr l)all? env query-mode? )
                     (format t "~2%No more solutions"))))
              (all?
               (let ((envs 
                      (if (cdr l)
		          (Let ((result2 (ask-or (cdr l)all? env query-mode? )))
                            (if (eq result2 :fail)
                                result
                                (if (eq result :fail)
                                    result2
                                    (nconc result result2))))
                          result)))
                 (if (Listp envs)
                     (remove-duplicates envs :test #'equal)
                     envs)))
              ((eq result :fail)
               (if (cdr l)
		   (ask-or (cdr l)all? env query-mode? )
		   :fail))
              (t
               result)))
      l))
 
;(defun ask-or (l &optional all? env query-mode? )
;  (if l
;      (let ((result (ask-internal (car l) all?  env query-mode? )))
;        (if (eq result :fail)
;            (if (cdr l)
;                (ask-or (cdr l)all? env query-mode? )
;                :fail)
;            result))
;      l))

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


        
          
