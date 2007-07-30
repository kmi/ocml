default:
	@echo "There is no default target."

clean:
	find . -name "*.fasl" -o -name "*.fsl" -o -name "*.pfsl" -o -name "*.ufasl" -o -name "*.wfasl" | xargs rm -f
	$(MAKE) -C doc clean

tests: acl-tests lw-tests sbcl-tests

sbcl-tests:
	cat tests/run-tests.lisp | sbcl

lw-tests:
	cat tests/run-tests.lisp | lw-console

acl-tests:
	cat tests/run-tests.lisp | alisp
