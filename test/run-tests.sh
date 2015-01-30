#!/usr/bin/env bash

set -e

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch \
       $([[ $EMACS == "emacs23" ]] && echo -l ert.el) \
       -l cl-lib.el \
       -l xtest.el \
       -l ../elfeed-org.el \
       -l elfeed-org-test.el \
       -f ert-run-tests-batch-and-exit

if [[ $EMACS != "emacs23" ]]; then
    $EMACS -Q --batch \
           --eval '(setq byte-compile-error-on-warn t)' \
           -f batch-byte-compile ../elfeed-org.el
fi
