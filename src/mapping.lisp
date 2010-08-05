;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


;;;;(defvar *mapping-rule-packet-name* :downward-mapping-rules)

;(defun make-mapping-rule-packet-name ()
;  (read-from-string(string-append (string (name *current-ontology*))"-DOWNWARD-MAPPING-RULES")))



(defun def-rel-mapping-internal (relation direction documentation clauses )
  (unless (stringp documentation)
    (setf clauses (cons documentation clauses)
          documentation nil))
  (cond ((eq direction :up)
	 (add-backward-rule relation  documentation clauses)
         (setf (upward-mapping? (get-relation relation))t))
          ((or (eq direction :down)
               (equal direction '(:down :add)))
           (set-downward-add-exp relation (car clauses) ))
          ((equal direction '(:down :remove))
           (let*((exp (car clauses))
                 (l (length (second exp))))
           (setf (downward-remove-exp (find-or-create-relation relation l))
                 exp)))))

(defun set-downward-add-exp (relation exp )
  (setf (downward-add-exp
         (find-or-create-relation relation (length (second exp))))
	exp))

;;;TRIGGER-DOWNWARD-MAPPING-EXP ---A downward relation mapping is an anonymous procedure
(defun trigger-downward-mapping-exp (exp args &optional env)
  (apply-lambda-exp exp args env #'procedure-eval nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-upward-class-mapping-internal (domain-class 
                                          method-class 
                                          name-function
                                          explicit-link 
                                          mapping-relation)
  (loop for x in (setofall '?x (list domain-class '?x))
        for meta-x = (funcall name-function x method-class)
        do
        (define-domain-instance 
          meta-x
          method-class)
        (when explicit-link
          (tell1 (List mapping-relation
                       meta-x x)))))
#|moved to top8.lisp to help with compilation
john domingue may 21 '98
(defun default-upward-naming-function (domain-name method-class)
  (read-from-string 
   (string-append
    (string method-class)
    "-"
    (string domain-name))))
|#




;;;;;;OLD STUFF BELOW THIS LINE;;;;;;;;    

;(defun set-downward-mapping-rule (relation clause  documentation)
;  (Let* ((name (gentemp (string-append  "MAPPING-RULE-OF-" (string relation))))
;	 (then-pos (position 'then clause))
;	 (rule (add-forward-rule name 
;				 documentation
;				 *default-fc-rule-priority*
;				 (get-or-create-packet name) ;;;;packet-name
;				 clause
;				 then-pos)))
;    (compile-fc-rule rule
;                     (subseq clause 0 then-pos)
;                     (subseq clause (1+ then-pos)))
;    (setf (downward-mapping-rule (get-relation relation))
;	  ;;;;;(find-or-create-relation relation 1))
;	  rule)
;    (trigger-downward-mapping-rule rule)))
                  


;(defun maybe-trigger-downward-mapping-rule (relation)
;  (let ((rule (downward-mapping-rule  (get-relation relation))))
;    (when rule
;      (trigger-downward-mapping-rule rule))))
;
;(defun trigger-downward-mapping-rule (rule)
;  (do-interpreter-cycle (List rule)))
;


;(defun maybe-trigger-downward-add-exp (relation args &optional values)
;  (let ((exp (downward-add-exp(get-relation relation))))
;    (when exp
;      (cond (values
;             (loop for value in values
;                   do
;                   (trigger-mapping-exp exp (List args value))))
;            (t
;             (trigger-mapping-exp exp args))))))




;(defun maybe-trigger-downward-remove-exp (relation args &optional values)
;  (let ((exp (downward-remove-exp(get-relation relation))))
;    (when exp
;      (cond (values
;             (loop for value in values
;                   do
;                   (trigger-mapping-exp exp (List args value))))
;            (t
;             (trigger-mapping-exp exp args))))))



                  


                        
    
    