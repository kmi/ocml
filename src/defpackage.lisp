(defpackage ocml
  (:use common-lisp enrico-utilities)
  (:export *library-pathname*))

(eval-when (:load-toplevel)
 (push :ocml cl:*features*)
 (push :ocml-7.5 cl:*features*))
