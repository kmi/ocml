(in-package :ocml.tests)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ocml::initialize-ocml)
  (push `("ocml:library;**;*" "/home/djl/pristine/v5-1/**/*")
	(logical-pathname-translations "ocml"))
  (load "ocml:library;applications;apple-heuristic-classify-redux;load"))
