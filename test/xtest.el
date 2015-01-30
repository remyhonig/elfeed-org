;;; xtest.el --- Simple Testing with Emacs & ERT

;; Copyright (C) 2014 Mustafa Shameem

;; Author: Mustafa Shameem
;; Maintainer: Mustafa Shameem
;; URL: https://github.com/promethial/xtest
;; Created: November 2, 2014
;; Version: 1.1.0
;; Keywords: testing, ERT
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Simple Testing with Emacs & ERT
;;;
;;; For documentation refer to https://github.com/promethial/xtest

;;; Code:
(require 'ert)
(require 'cl-lib)

;;; XTest customization options

(defgroup xtest nil
  "Testing DSL built on top of ERT."
  :group 'languages)

(defcustom xt-cursor "-!-"
  "Representation of cursor point in buffer."
  :group 'xtest
  :type 'string)

;;; Internal functions have the namespace 'xtest'.
;;; User functions have the namespace 'xt' or 'xtd'.

;;; Buffer setup & testing helper functions.

(defun xtest-buffer-setup (initial-string)
  "Create a buffer with the contents of INITIAL-STRING and replace the first
xt-cursor appearance with actual cursor."
  (insert initial-string)
  (if (search-backward xt-cursor nil t)
      (replace-match "")
    (goto-char (point-min))))

(defun xtest-setup (initial-string action)
  "Insert INITIAL-STRING into a temporary buffer, replaces -!- with the cursor,
runs the function ACTION, and returns the contents of the buffer as a string.
If cursor position is not specified using -!-, the cursor is placed at the 
beginning of the temporary buffer."
  (with-temp-buffer
    (xtest-buffer-setup initial-string)
    (funcall action)
    (insert xt-cursor)
    (buffer-string)))

(defun xtest-return-setup (initial-string action)
  "Creates a temporary buffer with the contents of INTITIAL-STRING,
places the cursor where specified, and executes the function ACTION."
  (with-temp-buffer
    (xtest-buffer-setup initial-string)
    (funcall action)))

;;; Core XTest
;;; Internal representation will be not be useful for the user in
;;; understanding how to use the XTest framework.

(defvar xtest-external-internal-func-map '((xt-should . xtest-should)
                                           (xt-should! . xtest-should-not)
                                           ;; Data specific function mappings
                                           (xtd-should . xtest-data-should)
                                           (xtd-should! . xtest-data-should-not)
                                           ;; Buffer specific function mappings
                                           (xtd-setup= . xtest-data-setup=)
                                           (xtd-return= . xtest-data-return=))
  "Mapping user facing functions to internal macro representations.")

(defun xtest-construct-test-name (test-name test-num)
  "Returns a unique test symbol name based on the overall test name
appended with a dash and a number to the end of the test name."
  (intern (concat (symbol-name test-name) "-" (number-to-string test-num))))

(defun xtest-construct-test (name test-number tests test-expression)
  "Constructs an ert-deftest for each test sepecified in a test group."
  `(progn ,@(mapcar (lambda (test) `(ert-deftest ,(xtest-construct-test-name name (cl-incf test-number))
                                        () "" ,(funcall test-expression test)))
                    tests)))

(defun xtest-should= (test1 test2)
  "Test equality of TEST1 and TEST2."
  `(should (equal ,test1 ,test2)))

;;; Test Functions
;;; User Facing Functions

(defun xt-should (&rest tests)
  "Asserts all test expressions in TESTS evaluate to true.
Each expression will be expanded into a separate ert-deftest.")

(defun xt-should! (&rest tests)
  "Asserts all test expressions in TESTS evaluate to nil.
Each expression will be expanded into a separate ert-deftest.")

;;; Data Functions

(defun xtd-should (test-function &rest tests)
  "Asserts when TEST-FUNCTION is applied to each test in TESTS this returns true.
The TEST-FUNCTION must accept as many arguments as each test supplies.")

(defun xtd-should! (test-function &rest tests)
  "Asserts when TEST-FUNCTION is applied to each test in TESTS this returns nil.
The TEST-FUNCTION must accept as many arguments as each test supplies.")

;;; Buffer Functions

(defun xtd-setup= (test-function &rest tests)
  "TEST-FUNCTION is applied to each temporary buffer created by tests.
The resulting buffer is turned back into a string with the cursor replaced with
xt-cursor. The resulting string is asserted to see if it is equal to the second
argument in the TESTS. Each test in tests must have the form below. ")

(defun xtd-return= (test-function &rest tests)
  "TEST-FUNCTION is applied to each temporary buffer created by tests.
The value returned by TEST-FUNCTION is asserted to be equal to the second
argument in the test list. Equality is checked using the equal function.")

;;; Comment

(defun xt-note (&rest comments)
  "Is not processed by XTest and can be used leave comments or comment out other
test groups. ")

;;; Internal Functions / Actual Implementation

;;; General Test

(defun xtest-should (name test-num &rest tests)
  "Assert all the TESTS are true."
  (xtest-construct-test name test-num tests (lambda (test) `(should ,test))))

(defun xtest-should-not (name test-num &rest tests)
  "Assert all the TESTS are nil."
  (xtest-construct-test name test-num tests (lambda (test) `(should-not ,test))))

;;; Data Based Test

(defun xtest-data-should (name test-num test-function &rest tests)
  "Assert all the TESTS are true when TEST-FUNCTION is applied to each test in TESTS."
  (xtest-construct-test name test-num tests (lambda (test) `(should (apply ,test-function ',test)))))

(defun xtest-data-should-not (name test-num test-function &rest tests)
  "Assert all the TESTS are nil when TEST-FUNCTION is applied to each test in TESTS."
  (xtest-construct-test name test-num tests (lambda (test) `(should-not (apply ,test-function ',test)))))

;;; Buffer & Data Based Test

(defun xtest-data-setup= (name test-num test-function &rest tests)
  "Assert the buffer produced by applying TEST-FUNCTION in the buffer specified by
each test in TESTS is equal to the expected buffer output."
  (xtest-construct-test name
                        test-num
                        tests
                        (lambda (test) (xtest-should= `(xtest-setup ,(cl-first test)
                                                               (lambda () (funcall ,test-function
                                                                              ',(cl-third test))))
                                                 (cl-second test)))))

(defun xtest-data-return= (name test-num test-function &rest tests)
  "Assert the value returned by applying TEST-FUNCTION to each test in TEST is true."
  (xtest-construct-test name
                        test-num
                        tests
                        (lambda (test) (xtest-should= `(xtest-return-setup ,(cl-first test)
                                                                      (lambda () (funcall ,test-function
                                                                                     ',(cl-third test))))
                                                 (cl-second test)))))

;;; Test Composer / Constructor Macro

(defmacro xt-deftest (base-test-name &rest test-groups)
  "Allows creation of grouped tests."
  (declare (indent 1))
  (let ((test-num 0)
        ;; Remove xt-note instances from TESTS list since they are not evaluated
        (actual-tests (cl-remove-if (lambda (test) (equal 'xt-note (cl-first test))) test-groups)))
    `(progn ,@(mapcar (lambda (x) (prog1 (apply (cl-rest (assq (cl-first x) xtest-external-internal-func-map))
                                           (append (list base-test-name
                                                         test-num)
                                                   (cl-rest x)))
                               (cl-incf test-num (if (string-match-p (regexp-quote "xtd")
                                                                     (symbol-name (cl-first x)))
                                                     (- (length (cl-rest x)) 1)
                                                   (length (cl-rest x))))))
                      actual-tests))))

;;; Font Lock

(defun xtest-keywords ()
  (font-lock-add-keywords nil
                          '(("\\<\\(xt-deftest\\)" . font-lock-keyword-face))))

(add-hook 'emacs-lisp-mode-hook 'xtest-keywords)

(provide 'xtest)
;;; xtest.el ends here
