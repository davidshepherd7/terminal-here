EMACS=emacs
CASK ?= cask

package-lint:
	cask exec $(EMACS) -Q --batch -l "package-lint.el" -f "package-lint-batch-and-exit" terminal-here.el

build : package-lint
	cask exec $(EMACS) -Q --batch --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" terminal-here.el

clean :
	@rm -f *.elc

test: build
	${CASK} exec ert-runner

install:
	${CASK} install

.PHONY:	all test install clean build
