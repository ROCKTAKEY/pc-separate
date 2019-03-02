EMACS ?= emacs
TESTINGFILE ?= separate-test.el
TESTEDFILES ?= separate.el

test:
	${EMACS} -batch -Q -L . -l ${TESTINGFILE} -f  ert-run-tests-batch-and-exit

travis:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${EMACS} -batch -Q -L . -eval "(batch-byte-compile)" separate.el

clean:
	rm -f separate.elc

.PHONY: test travis compile clean
