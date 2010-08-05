;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package ocml)

(in-ontology base-ontology)

;;;This contains some extra definition which have to do with 
;;;the OCML environment in general, such as interrupting 
;;;the interpreter, printing, etc....

(def-procedure OUTPUT (?msg &rest ?args)
   :constraint (string ?msg)
   :lisp-fun #'(lambda (msg &rest args)
                 (apply #'format t msg args)))

(def-function NEW-SYMBOL (?prefix)
  "Creates a new symbol"
   :constraint (string ?prefix)
   :lisp-fun #'(lambda (x)(gentemp (string x))))

(def-function NEW-VAR (?var)
  "gensyms a new var"
   :constraint (variable ?var)
   :lisp-fun #'(lambda (x)(gentemp (string x))))


(def-function the-name-of (?structure)
  :lisp-fun #'(lambda (x)
                (name x)))


(def-procedure BREAK (?msg &rest ?args)
   :lisp-fun #'(lambda (m &rest args)
                 (apply #'break m args)))

(def-procedure HALT-FC ()
   "Halts the forward chainer. It only makes sense if called when fc is running"
   :lisp-fun #'(lambda ()
                 (throw '%%halt%% :halt)))

(def-operator EXEC (?exp)
   "An operator to call arbitrary procedures"
   :lisp-fun #'(lambda (exp env)
                 (procedure-eval exp env)
                 (list env)))

;
;(def-procedure IN-ONTOLOGY (?exp ?proc)
;  :lisp-fun #'(lambda (exp proc )
;                (let ((current-ontology *current-ontology*))
;                  (unwind-protect 
;                    (progn 
;                      (select-ontology (ocml-eval-gen exp))
;                      (ocml-eval-gen proc))
;                    (switch-to-ontology current-ontology)))))
;

(def-operator HOLDS-IN-ONTOLOGY (?exp ?statement)
   "Checks whether ?statement holds in the ontology denoted by ?exp"
   :lisp-fun #'(lambda (exp statement env)
                 (let ((current-ontology *current-ontology*))
                  (unwind-protect 
                    (progn 
                      (select-ontology (ocml-eval-gen exp))
                      (ask-top-level statement :all t :env env))
                    (switch-to-ontology current-ontology)))))



(def-procedure FTELL (?fact)
   :body (tell ?fact)
   :lisp-fun #'(lambda (x )
                 (tell1 x)))

(def-procedure FUNASSERT (?fact)
   :body (unassert ?fact)) 

