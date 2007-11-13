;;; Dave Lambert, 2007

(in-package :ocml.tests)

(def-suite owl-suite
    :description "Tests for the OCML/OWL translation.")

(in-suite owl-suite)

(test create-owl-onto-test
  (finishes (ocml:translate :ocml :owl 'ocml::apples2 "./onto.owl"
                            :top-class 'ocml::apple
                            :namespace-prefix 'ocml::apples2
                            :namespace "http://www.world-of-apples.com/onto.owl")))


