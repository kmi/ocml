;;; Dave Lambert, 2007

;;; Needs the OCML extended library.

(in-package :ocml.tests)

(def-suite apples-suite
    :description "Tests for OCML, based on Enrico's apples ontology.")

(in-suite apples-suite)

(test classification-test
  (is (eq 'ocml::chinese-granny
	  (ocml:with-ontology ('ocml::apple-heuristic-classify-redux)
	    (ocml::apple-single-sol-classification
	     '((ocml::area ocml::china)
	       (ocml::background ocml::green)
	       (ocml::rusty ocml::no))))))
  (is (equal '(ocml::low-sweet-granny)
	     (ocml:with-ontology ('ocml::apple-heuristic-classify-redux)
	       (ocml::apple-optimal-classification
		'((ocml::background ocml::green)
		  (ocml::area ocml::china)
		  (ocml::sugar 30)))))))
