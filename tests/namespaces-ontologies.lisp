;;; XXX Need to try slot  and relations with #_ names.

(in-package :ocml)

(def-ontology pathology
    :author "dave"
    :namespace-uri "http://example.open.ac.uk/ontologies/pathology#"
    :files ())

(def-ontology oncology
    :author "dave"
    :namespace-uri "http://example.open.ac.uk/ontologies/oncology#"
    :includes (pathology)
    :files ())

(def-ontology zodiac
    :author "dave"
    :namespace-uri "http://example.open.ac.uk/ontologies/zodiac#"
    :files ())

(def-ontology dave-in-another-dimension
    :author "dave"
    :namespace-uri "http://example.open.ac.uk/ontologies/dave#"
    :includes (oncology zodiac)
    :files ())

(register-namespace "path" 'pathology)
(register-namespace "onco" 'oncology)
(register-namespace "zodiac" 'zodiac)

;;; Upper-level ways to be ill

(in-ontology pathology)

(def-class #_pathology)

(def-class #_disease (#_pathology))

(def-class #_disease-course)

(def-class #_acute (#_disease-course))

(def-class #_chronic (#_disease-course))

(def-class #_recurrent (#_disease-course))

;;; Ways to be ill with the Big C

(in-ontology oncology)

(def-class #_cancer (#_path:disease))

(def-class #_leukemia (#_cancer))

(def-class #_lymphocytic-leukemia (#_leukemia))

(def-class #_chronic-lymphocytic-leukemia (#_lymphocytic-leukemia #_path:acute))

(def-class #_acute-lymphocytic-leukemia (#_lymphocytic-leukemia #_path:chronic))

(def-class #_myelogenous-leukemia (#_leukemia))

(def-class #_acute-myelogenous-leukemia (#_myelogenous-leukemia #_path:acute))

(def-class #_chronic-myelogenous-leukemia (#_myelogenous-leukemia #_path:chronic))

;;; Ways to be ill in the head

(in-ontology zodiac)

(def-class nonsense)

(def-class star-sign (nonsense)
  ((approx-date :type string)
   (#_symbol :type string)))

(def-instance #_taurus star-sign
  ((approx-date "may")
   (#_symbol "bull")))

(def-instance #_cancer star-sign
  ((approx-date "june")
   (#_symbol "crab")))

(def-instance #_leo star-sign
  ((approx-date "august")
   (#_symbol "lion")))

;;; In a parallel universe, another Dave thinks differently...

(in-ontology dave-in-another-dimension)

(def-class #_person ()
  ((has-star-sign :type star-sign)))

(def-class #_superstitious)

(def-class #_superstitious-person (#_person #_superstitious))

(def-instance #_parallel-universe-dave #_superstitious-person
  ((has-star-sign #_zodiac:cancer)))

(def-relation prone-to-cancer (?person)
  :sufficient (and (#_person ?person)
		   (has-star-sign ?person #_zodiac:cancer)))

(def-relation symbols-of-sickness (?symbol)
  :sufficient (and (#_person ?person)
		   (has-star-sign ?person ?sign)
		   (#_zodiac:symbol ?sign ?symbol)))
