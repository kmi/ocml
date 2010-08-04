default:
	@echo "There is no default target."

clean:
	find . -name "*.fasl" -o -name "*.fsl" -o -name "*.pfsl" -o -name "*.ufasl" -o -name "*.wfasl" -o -name "*.xfasl" -o -name "*.fas" -o -name "*.lib" -o -name "*.lx64fsl" | xargs rm -f
	$(MAKE) -C doc clean

tests: acl-tests lispworks-tests sbcl-tests

### Tests

clisp-tests:
	cat tests/run-tests.lisp | clisp

sbcl-tests:
	cat tests/run-tests.lisp | sbcl

lispworks-tests lw-tests:
	cat tests/run-tests.lisp | lispworks-console

acl-tests:
	cat tests/run-tests.lisp | alisp

