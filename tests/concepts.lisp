;;; Dave Lambert, 2007

(in-package :ocml.tests)

(def-suite concepts-suite
    :description "Tests for OCML's concept reasoning.")

(in-suite concepts-suite)

(defun c (symbol)
  (ocml::get-domain-class symbol))

(test subsumption-test
  ;; Test non-reflexivity, transitivity.
  (is-true (ocml::subclass-of* (c 'ocml::integer) (list (c 'ocml::number))))
  (is-true (ocml::subclass-of* (c 'ocml::positive-integer) (list (c 'ocml::integer))))
  (is-true (ocml::subclass-of* (c 'ocml::positive-integer) (list (c 'ocml::number))))
  (is-false (ocml::subclass-of* (c 'ocml::number) (list (c 'ocml::integer))))
  (is-false (ocml::subclass-of* (c 'ocml::number) (list (c 'ocml::number))))

  (is (equalp (list (c 'ocml::number))
	      (ocml::remove-subsumed-classes (mapcar #'c '(ocml::number)))))
  (is (equalp (list (c 'ocml::number))
	      (ocml::remove-subsumed-classes
	       (mapcar #'c '(ocml::number ocml::integer ocml::positive-integer)))))
  (is (equalp (list (c 'ocml::number))
	      (ocml::remove-subsumed-classes
	       (mapcar #'c '(ocml::positive-integer ocml::integer ocml::number)))))
  (is (equalp (list (c 'ocml::number))
	      (ocml::remove-subsuming-classes (mapcar #'c '(ocml::number)))))
  (is (equalp (list (c 'ocml::positive-integer))
	      (ocml::remove-subsuming-classes
	       (mapcar #'c '(ocml::number ocml::integer ocml::positive-integer)))))
  (is (equalp (list (c 'ocml::positive-integer))
	      (ocml::remove-subsuming-classes
	       (mapcar #'c '(ocml::positive-integer ocml::integer ocml::number))))))
