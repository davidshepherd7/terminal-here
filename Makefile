EMACS=emacs
CASK ?= cask

build :
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
