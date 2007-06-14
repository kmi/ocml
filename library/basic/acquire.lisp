;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

(in-package "OCML")

(in-ontology base-ontology)

(def-relation role-acquisition-documentation (?role ?task ?string)
  "This relation provides a way to associate documentation to a task role, to be used when
   acquiring role values"
  :constraint (and (role ?role) (task ?task)(string ?string)))