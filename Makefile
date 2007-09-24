default:
	@echo "There is no default target."

clean:
	find . -name "*.fasl" -o -name "*.fsl" -o -name "*.pfsl" -o -name "*.ufasl" -o -name "*.wfasl" -o -name "*.xfasl" | xargs rm -f
	$(MAKE) -C doc clean
	rm -rf /var/cache/common-lisp-controller/`id --user`/*/local`pwd`

tests: acl-tests lispworks-tests sbcl-tests

sbcl-tests:
	cat tests/run-tests.lisp | sbcl

lispworks-tests lw-tests:
	cat tests/run-tests.lisp | lispworks-console

acl-tests:
	cat tests/run-tests.lisp | alisp
