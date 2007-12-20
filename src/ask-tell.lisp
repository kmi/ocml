;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package ocml)

(defun and-expression? (exp)
  (and (listp exp)
       (eq (car exp )'and)))


(defun ask-internal (query &optional all env query-mode compiled )
  (let ((pred (car query)))
    (when (variable? pred)
      (multiple-value-bind (flag binding)
          (lookup pred env)
        (if flag
            (Setf pred binding)
            (error "Can't handle query ~S in env. ~s" query env))))
    (case pred
      (not
       (setf query (maybe-canonicalize-not query))
       (prove (list query):all all :env env :query-mode query-mode :compiled compiled
              ))
      (and
       (prove (cdr query) :all all :env env :query-mode query-mode :compiled compiled
              ))
      ;;;;;;(or (ask-or (cdr query)all env query-mode compiled ))
     
      (otherwise
       (prove (list query) :all all :env env :query-mode query-mode :compiled compiled
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

      
;;;Definition by mauro - modified by me

(defun prove (goals &key all env query-mode compiled)
  compiled ;;not used
  (prove-template-int goals :all all :env env :query-mode query-mode))

;;(if compiled
;;    (prove-template-cmp goals :all all :env env :query-mode query-mode)
 ;;   (prove-template-int goals :all all :env env :query-mode query-mode)))
    
      

;;;This is my old and trusted definition
;;;PROVE ---Proves a list of goals. It returns
;;;Nil if query-mode? is t
;;;One env if all is nil
;;;A list of envs if all is t
;(defun prove (goals &key all env query-mode compiled )
;  (let ((bindings (*prove goals :all all :env env :query query-mode )))
;    (if query-mode?
;        bindings
;      (if bindings
;	  (if all
;              bindings
;              (car bindings))
;          :fail))))


(defun prove-template-int (goals &key all env query-mode compiled)
  compiled ;;;ignore
  (*prove goals :all all :env env :query query-mode))

 
 ; (multiple-value-bind (result cont)
 ;                      (*prove goals :all all :env env :query query-mode)
 ;   (if query-mode
 ;       result
 ;       (if result
 ;	  (if all
 ;           result
 ;           (values result cont))
 ;         :fail))))

(defun prove-template-cmp (goals &key all env query-mode compiled)
  compiled ;;;ignore
  (let ((bindings (eval `(call-bc-rule ,goals ,all ,env ,query-mode ;;;;;;nil
                                       nil))))
    (if query-mode
        bindings
      (if bindings
	  (if all
              bindings
              (car bindings))
          :fail))))


;;;ASK-OR ---Succeeds if one of the goals in <l> is satisfied
(defun ask-or (l &optional all? env query-mode? compiled )
  (if l
      (let ((*inside-or-query* t))
        (multiple-value-bind (result cont)
	                     (ask-internal (car l) all?  env query-mode? compiled)
        (cond (query-mode?
	       (unless (eq result :quit)
		 (if (cdr l)
		     (ask-or (cdr l)all? env query-mode? compiled)
                     (format t "~2%No more solutions"))))
              (all?
               (let ((envs 
                      (if (cdr l)
		          (Let ((result2 (ask-or (cdr l)all? env query-mode? compiled)))
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
		   (ask-or (cdr l)all? env query-mode? compiled)
		   :fail))
              (t
               (values result cont)))))
      l))
 

;(defun ask-exists-query (vars query &optional env query-mode? compiled )
;  (multiple-value-bind (env cont)
;                       (ask-internal query nil env nil compiled)
;    (unless (Listp vars)
;      (setf vars (list vars)))
;    (if (eq env :fail)
;        (if query-mode?
;            (format t "No (more) solutions")
;            :fail)
;	(Let ((new-env
;	       (mapcar #'(lambda (var)
;                           (multiple-value-bind (flag binding)
;			       (lookup var env)
;                             (declare (ignore flag))
;		             (cons var binding)))
;                       vars)))
;          (if query-mode?
;              (format t "~2%Solution: ~{~S ~}"new-env)
;	      (values new-env cont))))))
;

        
          
