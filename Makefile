EMACS ?= emacs
CASK ?= cask

CASKEMACS = $(CASK) exec $(EMACS)
LOAD-ORGMODE = -L org-mode/lisp

# http://stackoverflow.com/questions/3931741/why-does-make-think-the-target-is-up-to-date
.PHONY: install build test clean

all: install build test clean

# install dependencies
install:
	EMACS=${EMACS} $(CASK) install

build:
	EMACS=${EMACS} $(CASK) build

test:
	EMACS=${EMACS} $(CASK) exec ert-runner 

clean:
	EMACS=${EMACS} $(CASK) clean-elc
