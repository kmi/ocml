(in-package :ocml.tests)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ocml:initialize-ocml)
  (load "ocml:library;applications;apple-heuristic-classify-redux;load"))
