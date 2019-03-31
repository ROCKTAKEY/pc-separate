EMACS ?= emacs
TESTINGFILE := separate-test.el
TESTEDFILES := separate.el
CASK ?= cask

test:
	${CASK} exec ${EMACS} -batch -Q -L . -l ${TESTINGFILE} -f  ert-run-tests-batch-and-exit

travis:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -batch -Q -L . -eval "(batch-byte-compile)" system-separate.el

clean:
	rm -f separate.elc

.PHONY: easy-test test travis compile clean
