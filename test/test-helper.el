;; Remove standard org-mode paths so I can supply them with my own
;; using the "cask exec ert-runner -L org-mode/lisp" command
(setq load-path (-remove (lambda (path) (s-ends-with? "lisp/org" path)) load-path))
(message "Org version: %s on Emacs version: %s" (org-version) (emacs-version))

(require 'xtest)
(load-file "elfeed-org.el")

;;; Code:

(defmacro with-fixture (file &rest body)
  "Use FILE as current `org-mode' buffer to run the BODY."
  ()
  `(with-temp-buffer
     (insert-file-contents (expand-file-name ,file))
     (org-mode)
     ,@body))

(defun xt-trees-with-id-length (fixture expected-length)
  "Compare length of trees.
Argument FIXTURE An org file.
Argument EXPECTED-LENGTH The number of trees found in the FIXTURE."
  (= (safe-length (with-fixture fixture (rmh-elfeed-org-import-trees "elfeed")))
     expected-length))

(defun xt-feeds (fixture expected)
  "Compare result of trees.
Argument FIXTURE An org file.
Argument EXPECTED the expected feeds list."
  (let ((actual (with-fixture fixture
                              (rmh-elfeed-org-filter-relevant
                               (rmh-elfeed-org-convert-tree-to-headlines
                                (rmh-elfeed-org-import-trees "elfeed"))))))
    
    (equal actual expected)))
