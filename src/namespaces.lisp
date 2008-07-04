;;; Namespace support for OCML.
;;;
;;; Copyright © 2007, 2008 The Open University.
;;;
;;; Authored by Dave Lambert

;;; We use symbol munging to implement namespaces, not package
;;; manipulation.  Although the Lisp symbol "foo:bar" looks like the
;;; XML element "<foo:bar>", they're fundamentally different.  In XML,
;;; the namespaces mapped to from the various prefixes can, and do,
;;; change, even within the same document.  In Lisp, the package name
;;; is forever, and we can't have that: if two sources were to use the
;;; same prefix, they must use it for the *same ontology*, and that
;;; simply isn't what we need.

(in-package :ocml)

;;; Should be the chars acecptable in a an XML/RDF/OWL token.
(define-constant +token-chars+
    (concatenate 'list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
  "Legal characters in a namespaced symbol's 'local name'.")

(define-constant +valid-whitespace+
    (list #\space #\return #\linefeed #\tab #\( #\) #\' #\` #\")
  "Characters which can legitimately follow a valid token.")

(define-constant +namespace-separator+ #\:)

(defun register-namespace (prefix namespace)
  (check-type prefix string)
  (check-type namespace (or string symbol))
  (setf (prefix->uri prefix)
        (if (symbolp namespace)
            (namespace-uri-of
             (get-ontology namespace :error-if-not-found t))
            namespace)))

(defun assumed-namespace-uri (ontology-name)
  "Generate a unique namespace for ontologies which don't explicitly declare one."
  (format nil "http://www.kmi.open.ac.uk/projects/ocml/namespaces/assumed/~A#"
          ontology-name))

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
               ((member char +valid-whitespace+) (return))
	       ((not (ocml-token-char? char))
                (error "Illegal character '~S' in OCML token '~A'."
                       char (concatenate 'string (reverse chars))))
	       (t (push char chars)
		  (read-char stream))))
      ;; If there's a prefix, check it's valid, map it to an ontology.
      ;; If there's no prefix, use *CURRENT-ONTOLOGY*.
      (let ((namespace (if prefix
                           (or (prefix->uri prefix)
                               (error "Unrecognised namespace prefix \"~A\"."
                                      prefix))
                           (namespace-uri-of *current-ontology*)))
	    (symbol (concatenate 'string (reverse chars))))
	(format nil "~A~A" namespace symbol)))))

(defun ocml-token-char? (char)
  (member char +token-chars+))

(defun prefix->uri (prefix)
  "Lookup the namespace IRI that PREFIX currently maps to."
  (cdr (assoc prefix *namespace-prefixes* :test #'string=)))

(defun (setf prefix->uri) (iri prefix)
  ;; We don't allow a URL to have more than one prefix defined for it,
  ;; so remove any such mapping first.
  (setf *namespace-prefixes* (remove-if (lambda (x) (string= iri (cdr x)))
                                        *namespace-prefixes*))
  (let ((pair (assoc prefix *namespace-prefixes* :test #'string=)))
    (if pair
	(setf (cdr pair) iri)
	(push (cons prefix iri) *namespace-prefixes*)))
  prefix)

(defun merge-included-namespaces (ontologies)
  "Calculate the prefixes to be used in an ontology including
ONTOLOGIES."
  ;; If a namespace has more than one prefix, we just choose the first
  ;; one.  If a prefix refers to more than one namespace, we alter the
  ;; prefixes to be unique.
  (let ((unique-number 0)
        (*namespace-prefixes* '())
        (mappings (apply #'append (mapcar (lambda (o) (namespaces-of (get-ontology o)))
                                          ontologies))))
    (dolist (map mappings)
      (let ((prefix (first map))
            (namespace (let ((ns (second map)))
                         (if (symbolp ns)
                             (namespace-uri-of (get-ontology ns))
                             ns))))
        (unless (member namespace *namespace-prefixes* :key #'cdr)
          (when (member prefix *namespace-prefixes* :key #'car)
            (setf prefix (format nil "~A~A" prefix (incf unique-number))))
          (register-namespace prefix namespace))))
    *namespace-prefixes*))

(eval-when (:load-toplevel :execute)
  (set-dispatch-macro-character #\# #\_ #'read-wsml-identifier))

;;; This is a preview of correctly handling OCML namespaces.  I'm
;;; mainly using it to mark places we need to check.
(defun extern-ocml-symbol (ocml-symbol)
  "Generate the external representation for an OCML symbol.

Second return value is true if a prefixed version was found."
  (let* ((symname (symbol-name ocml-symbol))
         (hash (position #\# symname)))
    (if hash
        (let* ((prefix (subseq symname 0 (+ 1 hash)))
               (localname (subseq symname (+ 1 hash)))
               (ns (namespace->prefix prefix)))
          (if ns
              (values (format nil "~A:~A" ns localname) t)
              (progn
                (unless *printing-namespaced-symbol*
                  ;; If we're *printing-namespaced-symbol**, then we
                  ;; don't want to bother the user with warnings.
                  ;; They can see immediately that there's no prefix.
                  (warn "No namespace prefix for symbol `~A' in ontology `~A'~%."
                        (symbol-name ocml-symbol) ocml::*current-ontology*))
                (values symname nil))))
        (values symname nil))))

(defun intern-ocml-symbol (ocml-external-string)
  "Generate the internal representation for an externalised OCML
symbol representation."
  (if (symbolp ocml-external-string)
      (setf ocml-external-string (symbol-name ocml-external-string)))
    (if (or (position #\# ocml-external-string)
            (not (position #\: ocml-external-string)))
        (intern ocml-external-string :ocml)
        (read-from-string (format nil "#_~A" ocml-external-string))))

(defun namespace->prefix (namespace)
  (car (rassoc namespace ocml::*namespace-prefixes* :test #'string=)))

(defun looks-like-namepaced-symbol? (symbol)
  "Returns true if SYMBOL looks like it's an OCML namespaced symbol."
  (let ((chars (symbol-name symbol))
        (prefix "http://"))
    (and (> (length chars) (length prefix))
         (string= "http://" (subseq chars 0 (length prefix))))))

#+:lispworks
(sys::without-warning-on-redefinition
  (defmethod print-object :around ((ocml-symbol symbol) stream)
    (if (and *pretty-print-namespaces*
             (looks-like-namepaced-symbol? ocml-symbol))
        (let ((*printing-namespaced-symbol* t))
          (multiple-value-bind (text prefixed?)
              (extern-ocml-symbol ocml-symbol)
            (format stream (if prefixed? "#_~A" "|~A|") text)))
        (call-next-method))))
