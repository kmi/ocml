default:
	@echo "There is no default target."

clean:
	find . -name "*.fasl" -o -name "*.fsl" -o -name "*.pfsl" -o -name "*.ufasl" -o -name "*.wfasl" | xargs rm -f
	$(MAKE) -C doc clean

tests: sbcl-tests

sbcl-tests:
	sbcl --eval "(require :ocml)" \
	    --eval "(without-package-locks (require :ocml-tests))" \
	    --eval "(ocml.tests:run-all-tests)" \
	    --eval "(cl-user::quit)"

lw-tests:
	echo "(asdf:operate 'asdf:load-op :ocml) \
		 (asdf:operate 'asdf:load-op :ocml-tests) \
		 (ocml.tests:run-all-tests)" \
	 | lw-console

acl-tests:
	echo "(asdf:operate 'asdf:load-op :ocml) \
		 (asdf:operate 'asdf:load-op :ocml-tests) \
		 (ocml.tests:run-all-tests)" \
	 | alisp


