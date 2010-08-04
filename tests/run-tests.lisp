;;; 2007 Open University

#+:ccl (require :asdf)

(push "." asdf:*central-registry*)

#+:lispworks5 (setf system:*stack-overflow-behaviour* :warn)

(asdf:operate 'asdf:load-op :ocml)

#+:sbcl
(without-package-locks
  (asdf:operate 'asdf:load-op :ocml-tests))

#-:sbcl
(asdf:operate 'asdf:load-op :ocml-tests)

(ocml.tests:run-all-tests)

#+:allegro (exit)
#+:ccl (quit)
#+:lispworks (quit)
