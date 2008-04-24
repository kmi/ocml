;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: OCML;   -*-

;; Time related Primitive Types
;; For a more advanced and complete Time Manipulation see the Time Ontology

(in-package "OCML")

(in-ontology base-ontology)

;; Exists in some examples and has no definition
(def-class list-date-and-time ()
  "A list (SECONDS MINUTES HOURS DAY MONTH YEAR)")

;;; 2007/04/12 Dave: New.  It's never been defined before, but
;;; invoking web services now requires that the declared types of
;;; arguments actually exist as OCML classes.  About time, too :-)
(def-class date-and-time ()
 "A list (DAY MONTH YEAR)")

;; Added by Carlos 24-2-2007
;; Added for supporting the transformation of durations in WSMO
(def-class list-duration ()
  "A list (SECONDS MINUTES HOURS DATE MONTH YEAR)")

