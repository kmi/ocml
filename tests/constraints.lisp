;;; Dave Lambert, 2007

;; Based on Enrico Motta's constraint checking example.

(in-package :ocml.tests)

(def-suite constraints-suite
    :description "Tests for OCML's constraint checking.")

(in-suite constraints-suite)

(defmacro generates-warnings (warnings &body form)
  "Returns T iff all WARNINGS have been seen."
  (let ((warningz (gensym))
	(result (gensym)))
    `(let ((,warningz ,warnings))
      (handler-bind
	  ((warning #'(lambda (c)
			(let ((expected (first ,warningz)))
			  (cond ((null expected)
				 (error "Got condition ~S when none was expected." c))
				((and (eq (class-of c) (find-class (first expected)))
				      (equal (simple-condition-format-arguments c)
					     (second expected)))
				 (pop ,warningz)
				 ;; And resume...
				 (muffle-warning c))
				(t
				 (describe c)
				 (error "Got error ~S when ~S was expected." c expected)))))))
	(let ((,result (progn ,@form)))
	  (if ,warningz
	      (error "Did not encounter expected warnings ~A." ,warningz)
	      ,result))))))

(test constraints-test
      (finishes (ocml:in-ocml (def-ontology persons-projects-dogs)))

      (is-true (ocml:in-ocml (enable-constraint-checking)))

      (is (eq (find-class 'ocml::ocml-metaclass)
	      (class-of (ocml:in-ocml (def-class person ())))))

      (is (eq (find-class 'ocml::ocml-metaclass)
	      (class-of (ocml:in-ocml (def-class dog ())))))

      (is (eq (find-class 'ocml::ocml-metaclass)
	      (class-of (ocml:in-ocml
	       (def-class project () ?x
		   ((has-project-leader :type person)
		    (has-project-soldier :type person))
		   ;;project leaders should be separated from 'project soldiers'
		   :constraint (forall ?p (=> (has-project-leader ?x ?p)
					      (not (has-project-soldier ?x ?p))))))) ))

      (is (eq (ocml:in-ocml (get-domain-class 'dog))
	      (class-of (ocml:in-ocml (def-instance fido dog)))))

      (is (eq (ocml:in-ocml (get-domain-class 'person))
	      (class-of (ocml:in-ocml (def-instance enrico person)))))

      (is (eq (ocml:in-ocml (get-domain-class 'project))
	      (class-of
	       (generates-warnings
		   (ocml:in-ocml '((<constraint-violation>
			       (ibrow (((KAPPA (?X)
					       (FORALL ?P (=> (HAS-PROJECT-LEADER ?X ?P)
							      (NOT (HAS-PROJECT-SOLDIER ?X ?P)))))
					PROJECT))))
			      (<constraint-violation>
			       (JOE-BLOGGS HAS-PROJECT-SOLDIER IBROW PERSON))
			      (<constraint-violation>
			       (mickey-mouse has-project-soldier ibrow person))
			      (<constraint-violation>
			       (joe-bloggs has-project-leader ibrow person))))
		 (ocml:in-ocml (def-instance IBROW project
			    ((has-project-leader joe-bloggs)
			     (has-project-soldier enrico mickey-mouse joe-bloggs))))))))

      (is (eq (ocml:in-ocml (find-class 'ocml-relation))
	      (ocml:in-ocml (class-of (def-relation involved-in-project (?x ?project)
			   "This relation associates people to projects"
			   :constraint (and (person ?x)
					    (project ?project)))))))

      (is (equal (ocml:in-ocml '(involved-in-project fido ibrow))
		 (generates-warnings
		     (ocml:in-ocml '((<constraint-violation>
				 ((INVOLVED-IN-PROJECT FIDO IBROW)
				  ((KAPPA (?X ?PROJECT) (AND (PERSON ?X) (PROJECT ?PROJECT))))))))
		   (ocml:in-ocml (tell (involved-in-project fido ibrow))))))

      (is  (equal (ocml:in-ocml '(INVOLVED-IN-PROJECT ENRICO IBROW))
		  (ocml:in-ocml (tell (involved-in-project enrico ibrow)))))

      (is-false (ocml:in-ocml (disable-constraint-checking))))
