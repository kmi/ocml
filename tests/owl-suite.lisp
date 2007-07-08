;;; Dave Lambert, 2007

(in-package :ocml.tests)

(def-suite owl-suite
    :description "Tests for the OCML/OWL translation.")

(in-suite owl-suite)

(test create-owl-onto-test
  (finishes (ocml:with-ontology ('ocml::apples2)
	      (ocml::create-owl-onto
	       'ocml::apple 'ocml::apples2
	       "http://www.world-of-apples.com/onto.owl" "./onto.owl"))))
