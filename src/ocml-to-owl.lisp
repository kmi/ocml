;; OCML--> OWL translator

;; 2007 Michele Pasin

;; the translator at the moment is just a major help to port an ocml
;; ontology to owl you will probably have to manually review the
;; ontology - see below for detail


#|  

HOWTO:

1) load your ontology into OCML (translator works from within OCML)
2) compile&load the translator functions (this file)
3) call the translator with the appropriate arguments, e.g.
(create-owl-onto 'top-class-to-translate-from 'a-prefix "http://my-onto-namespace.owl" "local-path")

For example, in my case I used:
(create-owl-onto 'philosurfical-entity 'phil "http://philosurfical.open.ac.uk/ontology/philosurfical.owl"  "/Users/michelepasin/Desktop/-last-translation.owl")

What it does:
- it can  properly translate the TAXONOMY
- it creates a protege-readable owl file
- it transforms all the ocml-classes' slots into owl-OBJECT-PROPERTIES + owl-CLASS-RESTRICTIONS
...(interesting side-effect --> it helps checking the consistency of an OCML ontology!..typos, wrong args etc..)

What it doesn't do (for now):
- check for DATATYPEs, and translate them to XML-types
- translate complex restrictions
- translate contraints
- translate fixed values on ocml slots (in this case it outputs the same class as the property range)
- tanslate instances

|#


(in-package :ocml)


;; creates the header, the top class definition and the closing stuff
(defun create-owl-onto (topclass prefix namespace your-path)
  "Start function: sets up the translation constraints and creates the owl-headers"
  (let* ((contents (calcont topclass))
         (owl-onto 
          (format nil "~%<!DOCTYPE rdf:RDF [~%    <!ENTITY ~a  \"~a\" > ]> ~2% <rdf:RDF ~%   xmlns       =\"&~a;\"~%   xmlns:~a  =\"&~a;\"~%   xml:base    =\"&~a;\" ~%   xmlns:owl   =\"http://www.w3.org/2002/07/owl#\" ~%   xmlns:rdf   =\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" ~%   xmlns:rdfs  =\"http://www.w3.org/2000/01/rdf-schema#\" ~%   xmlns:xsd   =\"http://www.w3.org/2001/XMLSchema#\">  ~2%<owl:Ontology rdf:about=\"\">  ~%  <rdfs:comment>A translated ontology from OCML to OWL</rdfs:comment> ~%  <owl:priorVersion rdf:resource=\"\"/>  ~%</owl:Ontology> ~2%<owl:Class rdf:ID=\"~a\">~%  <rdfs:comment>~a~%  </rdfs:comment>~%</owl:Class>~%~a~2%</rdf:RDF>~%"
                  prefix namespace prefix prefix prefix prefix topclass (ocml-documentation (get-ocml-class topclass)) contents)))
    (with-open-file 
        (s your-path :direction :output :if-exists :supersede)
      (format s owl-onto))))

(defun calcont (top-class)
  "Main function that explores the tree"
  (let ((result ""))
    (if top-class
        (loop for class in (direct-subclasses (get-ocml-class top-class))
	   do (setq result (format nil "~a~%<owl:Class rdf:ID=\"~a\">~%  <rdfs:subClassOf rdf:resource=\"#~a\" />~%~a~%  <rdfs:comment>~a~%  </rdfs:comment>~%</owl:Class>~%~a~a"
				   result 
				   (name class) 
				   top-class 
				   (format nil "~a~%" (build-class-restrictions (name class)))
				   (ocml-documentation class)
				   (format nil "~a~%" (build-owl-properties (name class)))
				   (calcont (name class))))))
    result))

(defun build-class-restrictions (classname)
"Function that gets all the slots of a class, and transforms them into Owl-class-restrictions"
  (let ((result ""))
    (if classname
        (loop for rel in 
              (ocml-eval-gen `(local-class-slots ,classname))
              do 
              (setq result (format nil "~a~%  <rdfs:subClassOf>~%   <owl:Restriction>~%     <owl:onProperty rdf:resource=\"#~a\"/>~%     <owl:allValuesFrom>~%       <owl:Class rdf:ID=\"~a\"/>~%     </owl:allValuesFrom>~%   </owl:Restriction>~%  </rdfs:subClassOf>"  
                                   result 
                                   rel 
                                   (if   ;; for now 
                                       (first (get-slot-type (get-ocml-class classname) rel))
                                       (first (get-slot-type (get-ocml-class classname) rel))
                                     classname)  ;;if values an instance, puts back the class
                                   ))))
    result))


(defun build-owl-properties (classname)
"Function that gets all the slots of a class, and transforms them into Owl-ObjectProperties"
  (let ((result ""))
    (if classname
        (loop for rel in 
              (ocml-eval-gen `(local-class-slots ,classname))
              do 
              (setq result (format nil "~a~%<owl:ObjectProperty rdf:ID=\"~a\">~%  <rdfs:domain rdf:resource=\"#~a\" />~%</owl:ObjectProperty>" 
                                   result 
                                   rel 
                                   classname 
                                  ;; (if   ;; for now *
                                  ;;     (first (get-slot-type (get-ocml-class classname) rel))
                                  ;;     (first (get-slot-type (get-ocml-class classname) rel))
                                  ;;   classname)
                                   ))))
    result))

;; * we are not translating the RANGE of a property, for now, as the OWL semantics differs a lot 
;;  from OCML, in this respect // (the html code was:  ~%  <rdfs:range rdf:resource=\"#~a\" /> )

                                   

;; on (first (get-slot-type (get-ocml-class classname) rel) we will have to check whether it's an objectProperty, or a dataType, or a fixed value!

#|

ALGORiTHM (not updated):

take top-class
get subclasses
for each class
write class-owl
get the documentation, translate the documentation
close class-owl
call translate slots
return
 
for each class:
get the list of local slots
for each slot
get the slot type
write slot-owl-ObjectProperties

 |#
