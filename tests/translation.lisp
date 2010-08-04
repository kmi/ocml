;;; Dave Lambert, 2007, 2008.

(in-package :ocml.tests)

(def-suite translation-suite
    :description "Tests for the various OCML translations.")

(in-suite translation-suite)

(defun ontolingua-load-file (ontology-name)
  (format nil "~Aontolingua/load.onto" (ocml:find-ontology-directory ontology-name)))

(test translate-to-ontolingua-test
  (ocml::rm-file (ontolingua-load-file 'ocml::apples-reduced))
  (finishes (ocml:translate :ocml :ontolingua 'ocml::apples-reduced
                            :top-class 'ocml::apple))
  ;; Check the files have been written
  (finishes (open (ontolingua-load-file 'ocml::apples-reduced))))

(test translate-to-owl-test
  (finishes (ocml:translate :ocml :owl 'ocml::apples-reduced
                            :where "./onto.owl"
                            :top-class 'ocml::fruit
                            :namespace-prefix 'ocml::apples-reduced
                            :namespace "http://www.world-of-apples.com/onto.owl")))
