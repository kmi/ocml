;;; 2007 Open University

#+:lispworks5 (setf system:*stack-overflow-behaviour* :warn)

(asdf:operate 'asdf:load-op :ocml)

#+:sbcl
(without-package-locks
  (asdf:operate 'asdf:load-op :ocml-tests))

#-:sbcl
(asdf:operate 'asdf:load-op :ocml-tests)

(ocml.tests:run-all-tests)

#+:lispworks (quit)
#+:allegro (exit)
