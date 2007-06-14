? (def-ontology persons-projects-dogs)

NIL

? (enable-constraint-checking)

T

? (def-class person ())

#<OCML-METACLASS PERSON0>

? (def-class dog ())

#<OCML-METACLASS DOG0>

? (def-class project () ?x
  ((has-project-leader :type person)
   (has-project-soldier :type person))
  ;;project leaders should be separated from 'project soldiers'
  :constraint (forall ?p
                      (=> (has-project-leader ?x ?p)
                          (not (has-project-soldier ?x ?p)))))
 

#<OCML-METACLASS PROJECT0>

? (def-instance fido dog)

#<DOG FIDO>

? (def-instance enrico person)

#<PERSON ENRICO>

? (def-instance IBROW project
  ((has-project-leader joe-bloggs)
   (has-project-soldier enrico mickey-mouse joe-bloggs)))

;;;oopss..joe bloggs is both a leader and a soldier...



Warning: "Instance IBROW of class PROJECT violates the following constraints:
 (KAPPA (?X) (FORALL ?P (=> (HAS-PROJECT-LEADER ?X ?P) (NOT (HAS-PROJECT-SOLDIER ?X ?P))))) inherited from class PROJECT
"

;;;...and who is mickey mouse?

Warning: "The value MICKEY-MOUSE of slot HAS-PROJECT-SOLDIER of IBROW violates type constraint PERSON"

;;;...and who is joe bloggs?

Warning: "The value JOE-BLOGGS of slot HAS-PROJECT-LEADER of IBROW violates type constraint PERSON"
#<PROJECT IBROW>

? (def-relation involved-in-project (?x ?project)
 "This relation associates people to projects"
  :constraint (and (person ?x)
                   (project ?project)))

<OCML-RELATION INVOLVED-IN-PROJECT>

? (tell (involved-in-project fido ibrow))

;;oops...fido is not a person....

Warning: "Relation instance (INVOLVED-IN-PROJECT FIDO IBROW) violates the following applicable constraints: 
 (KAPPA (?X ?PROJECT) (AND (PERSON ?X) (PROJECT ?PROJECT)))
"
(INVOLVED-IN-PROJECT FIDO IBROW)

? (tell (involved-in-project enrico ibrow))


(INVOLVED-IN-PROJECT ENRICO IBROW)
