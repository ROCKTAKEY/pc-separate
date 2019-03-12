EMACS ?= emacs
TESTINGFILE := system-separate-test.el
TESTEDFILES := system-separate.el
CASK ?= cask

easy-test:
	${EMACS} -batch -Q -L . -l ${TESTINGFILE} -f  ert-run-tests-batch-and-exit

test:
	${CASK} exec ert-runner

travis:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${EMACS} -batch -Q -L . -eval "(batch-byte-compile)" system-separate.el

clean:
	rm -f system-separate.elc

.PHONY: easy-test test travis compile clean
