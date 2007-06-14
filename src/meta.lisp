;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ocml;   -*-

(in-package "OCML")


(defun decompose-meta-object (m-obj)
  (decompose-domain-object (denotation m-obj)))

(defun decompose-domain-object (d-obj)
    (if (atom d-obj)
      (meta-object d-obj)
      (cons (decompose-domain-object (car d-obj))
            (decompose-domain-object (cdr d-obj)))))
              

(defun recompose-meta-object (m-obj)
  (meta-object (denotation m-obj)))

(defun denotation (m-obj)
  (catch 'exit
    (denotation-2 m-obj)))

(defun  denotation-2 (m-obj)
   (if (stringp m-obj)
      (read-from-string m-obj)
      (if (listp m-obj)
        (cons (denotation-2 (car m-obj))
              (denotation-2 (cdr m-obj)))
        (throw 'exit :nothing))))

(defun meta-object (d-obj)
  (format nil "~s" d-obj))