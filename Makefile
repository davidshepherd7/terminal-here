package-lint:
	cask emacs -Q --batch -l "package-lint.el" -f "package-lint-batch-and-exit" terminal-here.el

checkdoc:
	cask emacs -Q --batch --eval '(checkdoc-file "terminal-here.el")'

build :
	cask emacs -Q --batch --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" terminal-here.el

clean :
	@rm -f *.elc

test: build
	cask emacs --batch -L . -L test -l terminal-here-test.el -f ert-run-tests-batch

install:
	cask install

.PHONY:	all test install clean build
