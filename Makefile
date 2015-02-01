# EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
EMACS = emacs

CASK = ~/.cask/bin/cask
CASKEMACS = $(CASK) exec $(EMACS)
LOAD = -l elfeed-org.el -l test/elfeed-org-test.el

# http://stackoverflow.com/questions/3931741/why-does-make-think-the-target-is-up-to-date
.PHONY: cask test

all: test

cask:
	$(shell EMACS=$(EMACS) $(CASK))

compile:
	$(CASKEMACS) -q  $(LOAD) elfeed-org.el \
	--eval "(progn (mapc #'byte-compile-file '(\"elfeed-org.el\")) (switch-to-buffer \"*Compile-Log*\") (ert t))"

test:
	$(CASKEMACS) -batch --eval="(message (concat \"Org version: \" (org-version) \" on Emacs version: \" (emacs-version)))"
	$(CASKEMACS) -batch $(LOAD) -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
