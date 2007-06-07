;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")

(defun def-expression-internal (name documentation assignment expression)
  (unless (stringp documentation)
    (setf assignment documentation
          expression assignment
          documentation ""))
  (unless (eq assignment ':=)
    (error "Bad syntax for expression ~S"
           expression))
  (new-expression name documentation   expression))

(defvar *expressions* nil)

(defun new-expression (name documentation  expression)
  (Let ((old (assoc name *expressions*)))
    (cond (old
           (warn "Redefining expression ~S"name)
           (setf (cdr old) (if documentation
                              (list expression documentation)
                              (list expression))))
          (t
           (setf  *expressions*
                  (acons name (if documentation
                              (list expression documentation)
                              (list expression))
                         *expressions*))))))

(defun get-expression (name)
  (first (cdr (assoc name *expressions*))))

(defun clear-expressions ()
  (setf *expressions* nil))


  