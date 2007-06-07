(defpackage enrico-utilities
  (:use common-lisp)
  (:nicknames eu utilities)
  (:export class-precedence-list
	   clear-subclasses-slot
	   create-directory
	   current-file
	   direct-subclasses
	   direct-superclasses
	   ensure-vanilla-class2
	   filter
	   init-arg-value
	   left-value
	   map-over-hash-table
	   mapcan*
	   pairify
	   print-with-spaces
	   remove-subclass
	   right-value
	   right-value*
	   set-equal
	   source-files
	   string-append
	   subclass-of?
	   subclasses
	   union*
	   ))

