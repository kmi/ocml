;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

;;;Some new code to enable the forward-chainer to run in 
;;;"watcher mode", that is, rules will fire as soon as 
;;;something triggers them, rather than as a result of 
;;;an explicit invocation of the function 'run'.



;;;*FC-IN-WATCHER-MODE*
(defvar *fc-in-watcher-mode* nil)


;;;CLASS RULE-PACKET
;;;If a rule packet, say p, is active, then when in watcher-mode,
;;;rule belonging to p will be compiled automatically.

(defclass rule-packet (name-mixin basic-ocml-object)
  ((hp-rules :initform nil :accessor rule-packet-hp-rules)
   (np-rules :initform nil :accessor rule-packet-np-rules)
   (lp-rules :initform nil :accessor rule-packet-lp-rules)
   (active? :initform nil :accessor rule-packet-active?)))


;;;ADD-FC-RULE
;;;Modified so that the rule is automatically compiled, if 
;;;it belongs to one of the current packets. 
(defun add-fc-rule (rule order priority packet)
  (prog1 
    (add-fc-rule-to-packet packet rule order priority)
    (when (and *fc-in-watcher-mode*
               (rule-packet-active? packet ))
      (compile-fc-rule rule (if-part rule)(then-part rule)))))


;;;ENABLE-FC-WATCHER-MODE
;;;Top level function to enable 'watcher mode'.  All packets 
;;;given as input are declared 'active', rules are compiled
;;;and instantiations are fired.  In addition, the flag 
;;;*fc-in-watcher-mode* is set to T, so that  new instantiations 
;;;will be fired as soon as they are created

(defun enable-fc-watcher-mode (&optional (packets :all))
  (let* ((packets (if (eq packets :all)
                    (all-rule-packets)
                    (mapcar #'get-or-create-packet packets)))
         (rules (merge-rules packets)))
  (mapcar #'(lambda (packet)
              (setf (rule-packet-active? packet) t))
          packets)
  (compile-fc-rules rules)
  (setf *fc-in-watcher-mode* t)
  (do-interpreter-cycle rules)))


;;;DISABLE-FC-WATCHER-MODE
;;;Disables 'watcher mode'

(defun disable-fc-watcher-mode ()
  (setf *fc-in-watcher-mode* nil))

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


