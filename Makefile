package-lint:
	cask exec emacs -Q --batch -l "package-lint.el" -f "package-lint-batch-and-exit" terminal-here.el

checkdoc:
	cask exec emacs -Q --batch --eval '(checkdoc-file "terminal-here.el")'

build :
	cask exec emacs -Q --batch --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" terminal-here.el

clean :
	@rm -f *.elc

test: build
	cask exec ert-runner

install:
	cask install

.PHONY:	all test install clean build
