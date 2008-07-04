;;; Dave Lambert, 2007

(in-package :ocml.tests)

(def-suite namespaces-suite
    :description "Tests for OCML's namespace support.")

(in-suite namespaces-suite)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *test-namespace-uri1* "http://www.example.com/ontology/One#")
  (defparameter *test-namespace-uri2* "http://www.acme.com/ontology/Two#")
  ;; DEF-ONTOLOGY doesn't evaluate its :namespace-uri argument, so we
  ;; have to paste in the actual string.  XXX This needs fixing!
  (ocml::def-ontology ocml::ontology-one
      :namespace-uri "http://www.example.com/ontology/One#" :files ())
  (ocml::def-ontology ocml::ontology-two
      :namespace-uri "http://www.acme.com/ontology/Two#" :files ()))

(test namespace-registration-test
  (finishes (ocml:register-namespace "uno" 'ocml::ontology-one))
  (is (string= *test-namespace-uri1* (ocml::prefix->uri "uno")))
  (finishes (ocml:register-namespace "one" 'ocml::ontology-one))
  (finishes (ocml:register-namespace "two" 'ocml::ontology-two))
  (is (eq nil (ocml::prefix->uri "uno")))
  (is (string= *test-namespace-uri1* (ocml::prefix->uri "one")))
  (is (string= *test-namespace-uri2* (ocml::prefix->uri "two"))))

;;; Since we're testing the reader, we shouldn't do the thing we're
;;; testing at read time.  Instead, read from strings at run-time.

(defun namespaced-symbol-ending-with (char)
  (format nil "#_foo~A" char))

(test (namespace-reader-test :depends-on namespace-registration-test)
  (is (eq (read-from-string "#_one:alpha")
	  (read-from-string "#_\"http://www.example.com/ontology/One#alpha\"")))
  (is (eq (read-from-string "#_one:alpha")
	  (intern (format nil "~A~A" *test-namespace-uri1* "alpha") :ocml)))
  (is (eq (read-from-string "#_one:Alpha")
	  (intern (format nil "~A~A" *test-namespace-uri1* "Alpha") :ocml)))
  (is (not (eq (read-from-string "#_one:Alpha") (read-from-string "#_one:alpha"))))
  (is (not (eq (read-from-string "#_one:Alpha") (read-from-string "#_two:Alpha"))))
  ;; Test correct handling of terminating characters.
  (signals error (read-from-string (namespaced-symbol-ending-with #\?)))
  (signals error (read-from-string (namespaced-symbol-ending-with #\!)))
  (signals error (read-from-string (namespaced-symbol-ending-with #\|)))
  (is (symbolp (read-from-string (namespaced-symbol-ending-with #\space))))
  (is (symbolp (read-from-string (namespaced-symbol-ending-with #\return))))
  (is (symbolp (read-from-string (namespaced-symbol-ending-with #\tab))))
  (is (symbolp (read-from-string (namespaced-symbol-ending-with #\( ))))
  (is (symbolp (read-from-string (namespaced-symbol-ending-with #\) ))))
  (is (symbolp (read-from-string (namespaced-symbol-ending-with #\' ))))
  (is (symbolp (read-from-string (namespaced-symbol-ending-with #\` ))))
  (is (symbolp (read-from-string (namespaced-symbol-ending-with #\" )))))

(test (intern-extern-symbols :depends-on namespace-reader-test)
  ;; Test intern-ocml-symbol and extern-ocml-symbol.
  (is-true (with-ontology ('ocml::pathology)
        (eq (read-from-string "#_path:disease")
            (intern-ocml-symbol "http://example.open.ac.uk/ontologies/pathology#disease"))))
  (is-true (with-ontology ('ocml::pathology)
             (eq (read-from-string "#_path:disease")
                 (intern-ocml-symbol "path:disease"))))
  (is-true (with-ontology ('ocml::pathology)
             (string= (extern-ocml-symbol (read-from-string "#_path:disease"))
                      "path:disease"))))

(test (namespace-ontologies-test :depends-on namespace-reader-test)
  (finishes (handler-bind ((warning #'muffle-warning))
	      ;; Suppress compiler warnings when we load the ontologies.
	      (load (merge-pathnames "tests/namespaces-ontologies.lisp"
				     (asdf:component-pathname
				      (asdf:find-system :ocml))))))
  (is (equal
       '(ocml::|http://example.open.ac.uk/ontologies/dave#parallel-universe-dave|)
       (with-ontology ('ocml::dave-in-another-dimension)
	 (ocml::ocml-eval-gen
	  '(ocml::setofall ocml::?x (ocml::prone-to-cancer ocml::?x))))))
  (is (equal '("crab")
	     (with-ontology ('ocml::dave-in-another-dimension)
	       (ocml::ocml-eval-gen
		'(ocml::findall ?x (ocml::symbols-of-sickness ?x)))))))

(test (namespace-intern/extern-test :depends-on namespace-ontologies-test)
  (is (string= "onco:cancer"
               (with-ontology ('ocml::dave-in-another-dimension)
                 (extern-ocml-symbol
                  '|http://example.open.ac.uk/ontologies/oncology#cancer|))))
  (is (eq 'ocml::|http://example.open.ac.uk/ontologies/oncology#cancer|
          (with-ontology ('ocml::dave-in-another-dimension)
            (intern-ocml-symbol "onco:cancer")))))

#+lispworks
(test pretty-print-test
  (is (string= "#_onco:cancer"
               (format nil "~A" (read-from-string "#_onco:cancer"))))
  (is (string= "http://example.open.ac.uk/ontologies/oncology#cancer"
               (let ((ocml:*pretty-print-namespaces* nil))
                 (format nil "~A" (read-from-string "#_onco:cancer")))))
  (is (string= "|http://example.open.ac.uk/ontologies/oncology#cancer|"
               (ocml:with-ontology ('ocml::base-ontology)
                 (format nil "~A" (ocml:with-ontology ('ocml::oncology)
                                    (read-from-string "#_onco:cancer")))))))

