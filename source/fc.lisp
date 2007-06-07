;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defclass rule-instantiation ()
  ((support :accessor instantiation-support :initarg :support)
   (ruled-out-by :accessor ruled-out-by)
   (body :initarg :body :accessor instantiation-body)))

;(defun make-instantiation (&rest args)
;  (apply #'make-instance
;         (cons 'rule-instantiation args)))

;;;MAKE-INSTANTIATION
;;;When in watcher mode, the new instantiation is immediately executed
(defun make-instantiation (rule &rest args)
  (let ((inst 
         (apply #'make-instance
                (cons 'rule-instantiation args))))
    (prog1 inst
      (when *fc-in-watcher-mode*
        (remove-fired-instantiation rule inst)
        (fire-instantiation inst rule )))))


(defmethod new-instantiations ((rule forward-rule)supports)
  (with-slots (instantiations instantiation-form) rule
    (setf instantiations
          (nconc  instantiations 
                  (mapcar #'(lambda (support)
		              (make-instantiation rule
                               :support support
			       :body (funcall instantiation-form support)))
			  supports)))))


(defmethod add-dummy-instantiation ((rule forward-rule))
  (with-slots (instantiations instantiation-form) rule
    (let ((support (make-dummy-support)))
      (setf instantiations (list
                            (make-instantiation rule
                             :support support
                             :body (funcall instantiation-form support)))))))



(defmethod remove-instantiations ((rule forward-rule)support-sets)
  (with-slots (instantiations)rule
    (setf instantiations
          (set-difference instantiations support-sets
			  :test #'(lambda (i s)
                                    (equal s (instantiation-support i)))))))

(defmethod remove-fired-instantiation ((rule forward-rule)(inst rule-instantiation))
  (with-slots (instantiations)rule
    (setf instantiations
          (remove inst instantiations))))

(defun make-dummy-support ()
  (list nil))

(defmethod generate-all-initial-instantiations ((rule forward-rule))
  (let ((end-node (fetch-end-node rule)))
    (if end-node
        (new-instantiations rule (beta-inputs end-node))
        (add-dummy-instantiation rule))))


(defun warn-about-free-vars-in-rhs? (operator)
  (member operator *tell-operators*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun do-interpreter-cycle (rules &optional (cycle 0))
;  (let (
;        (result
;         (catch '%%halt%% 
;           (loop 
;                 with *interpreter-running* = t
;                 do
;                 (multiple-value-bind (inst rule)
;                     (Select-instantiation rules)
;                   (cond (inst
;                          (remove-fired-instantiation rule inst)
;                          (fire-instantiation inst rule cycle)
;                          (incf cycle))
;                         (t
;                          (when *trace-fc*
;                            (format t "~2%No (more) rules can fire~2%~S cycles run" cycle))
;                          (return))))))))
;    (when (eq result :halt)
;      (format t "~2%Halt executed~2%~S cycles run" cycle))))



;;;DO-INTERPRETER-CYCLE
;;;Modified so that trace outputs is consistent with current
;;;mode (watcher or standard).
(defun do-interpreter-cycle (rules &optional (cycle 0))
  (let (
        (result
         (catch '%%halt%% 
           (loop 
                 with *interpreter-running* = t
                 do
                 (multiple-value-bind (inst rule)
                     (Select-instantiation rules)
                   (cond (inst
                          (remove-fired-instantiation rule inst)
                          (fire-instantiation inst rule cycle)
                          (incf cycle))
                         (t
                          (when (and *trace-fc* (not *fc-in-watcher-mode* ))
                            (format t "~2%No (more) rules can fire~2%~S cycles run" cycle))
                          (return))))))))
    (when (eq result :halt)
      (if *fc-in-watcher-mode*
        (format t "~2%Halt executed" )
        (format t "~2%Halt executed~2%~S cycles run" cycle)))))





    

(defun select-instantiation (rules)
  (loop for rule in rules
        for insts = (instantiations rule)
        until insts
        finally
        (return 
         (values (car insts) rule))))


;;;(defmethod  fire-instantiation ((inst rule-instantiation)(rule forward-rule)cycle)
;;;  (with-slots (body) inst
;;;    (when *trace-fc*
;;;      (format t "~2%Firing rule ~S at cycle ~S.....body is ~S" (name rule)cycle  body))
;;;    (prove body)))


;;;FIRE-INSTANTIATION
;;;Modified so that trace output is consistent with current
;;;mode (watcher or standard).
(defmethod  fire-instantiation ((inst rule-instantiation)(rule forward-rule) &optional cycle)
  (with-slots (body) inst
    (when *trace-fc*
      (if *fc-in-watcher-mode*
        (format t "~2%Firing rule ~S.....body is ~S" (name rule)  body)
        (format t "~2%Firing rule ~S at cycle ~S.....body is ~S" (name rule)cycle  body)))
    (prove body)))



