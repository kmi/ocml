;;; Namespace support for OCML.
;;;
;;; Copyright (C) 2007 The Open University.
;;;
;;; Authored by Dave Lambert

(in-package :ocml)

(defconstant +token-chars+
  (concatenate 'list
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-")
  "Should be the chars acecptable in a an XML/RDF/OWL token...
  Perhaps more.  i don't know :-(")

(defconstant +namespace-separator+ #\:)

(defun register-namespace (prefix ontology)
  (when (symbolp ontology)
    (setf ontology (get-ontology ontology)))
  (setf (prefix->ontology prefix) ontology))

(defun ocml-encode-symbol (symbol-name)
  (check-type symbol-name string)
  (intern symbol-name :ocml))

(defun read-wsml-identifier (stream char1 char2)
  (declare (ignore char1 char2))
  (let ((iri (char= #\" (peek-char nil stream))))
    (ocml-encode-symbol (if iri
			    (read stream)
			    (read-ocml-symbolic-thing stream)))))

(defun read-ocml-symbolic-thing (stream)
  (flet ((peek ()
	   (peek-char  nil  stream  nil :eof)))
    (let (char chars prefix)
      (loop (setf char (peek))
	 (cond ((eq char :eof)
		(return))
	       ((char= +namespace-separator+ char)
		(when prefix
		  (error "Multiple namespace prefixes found: \"~A\" and \"~A\"."
			 prefix (concatenate 'string (reverse chars))))
		(setf prefix (concatenate 'string (reverse chars)))
		(setf chars '())
		(read-char stream))
	       ((not (ocml-token-char? char)) (return))
	       (t (push char chars)
		  (read-char stream))))
      ;; If there's a prefix, check it's valid, map it to an ontology.
      ;; If there's no prefix, use *CURRENT-ONTOLOGY*.
      (let ((ontology (if prefix
			  (or (prefix->ontology prefix)
			      (error "Unrecognised namespace prefix \"~A\"."
				     prefix))
			  *current-ontology*))
	    (symbol (concatenate 'string (reverse chars))))
	(assert (namespace-uri-of ontology))
	(format nil "~A~A" (namespace-uri-of ontology) symbol)))))

(defun ocml-token-char? (char)
  (member char +token-chars+))

(defun prefix->ontology (prefix)
  "Lookup the ontology PREFIX currently maps to." 
  (cdr (assoc prefix *namespace-prefixes* :test #'string=)))

(defun (setf prefix->ontology) (ontology prefix)
  (let ((pair (assoc prefix *namespace-prefixes* :test #'string=)))
    (if pair
	(setf (cdr pair) ontology)
	(push (cons prefix ontology) *namespace-prefixes*)))
  ontology)

(eval-when (:load-toplevel :execute)
  (set-dispatch-macro-character #\# #\_ #'read-wsml-identifier))
