;;; elfeed-org-test.el --- Automated tests for elfeed-org
;(setq unit (expand-file-name "../elfeed-org.el"))

;;; Commentary:
;; 

(require 'xtest)
(require 'dash)
(require 's)

;;; Code:

(defun rmh-elfeed-org-trees-with-id-or-tagged (tree-id)
  "Get trees with \":ID:\" property or tag of value TREE-ID.
Return trees with TREE-ID as the value of the id property or
with a tag of the same value.  Setting an \":ID:\" property is not
recommended but I support it for backward compatibility of
current users."
  (org-element-map
      (org-element-parse-buffer)
      'headline
    (lambda (h)
      (when (or (member tree-id (org-element-property :tags h))
                (equal tree-id (org-element-property :ID h))) h))))

(defun rmh-elfeed-org-headlines-from-tree (match tree)
  "Return all headlines that match the regular expression in MATCH in TREE.
Argument TREE The structure returned by `org-element-parse-buffer'."
  (org-element-map
      tree
      'headline
    (lambda (h)
      (when (string-match match (org-element-property :raw-value h))
        (append
         (list (org-element-property :raw-value h))
         (mapcar 'intern (org-get-tags-at (org-element-property :begin h))))))))

(defun rmh-elfeed-org-remove-tag (feeds tag)
  "In all FEEDS given remove the TAG."
  (mapcar (lambda (e) (delete tag e)) feeds))

(defun rmh-elfeed-org-headliness-from-files (files tree-id match)
  "Visit all FILES and return the headlines stored under tree tagged TREE-ID or with the \":ID:\" TREE-ID in one list."
  (-distinct (-mapcat (lambda (file)
                        (with-current-buffer (find-file-noselect (expand-file-name file))
                          (org-mode)
                          (rmh-elfeed-org-remove-tag
                           (rmh-elfeed-org-headlines-from-tree match (rmh-elfeed-org-trees-with-id-or-tagged tree-id))
                           (intern tree-id))))
                      files)))

(defun rmh-elfeed-org-tagger-params (tagger-headline)
  "Add new entry hooks for tagging KEYWORDS."
  (list
   (s-trim (s-chop-prefix "entry-title:" (car tagger-headline)))
   (cdr tagger-headline)))

(defun rmh-elfeed-org-add-tagger-hook (tagger-headline)
  "Add new entry hook for TAGGER-HEADLINE."
  (let (params (rmh-elfeed-org-tagger-params tagger-headline))
    (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger
                                      :entry-title (car params)
                                      :add (cdr params)))))

;;; Test code:

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
  (= (safe-length (with-fixture fixture (rmh-elfeed-org-trees-with-id-or-tagged "elfeed")))
     expected-length))

(defun xt-feeds (fixture expected)
  "Compare result of trees.
Argument FIXTURE An org file.
Argument EXPECTED the expected feeds list."
  (let ((actual (with-fixture fixture
                              (rmh-elfeed-org-headlines-from-tree "http"
                               (rmh-elfeed-org-trees-with-id-or-tagged "elfeed")))))
    (prin1 "actual: ")
    (print actual)
    (prin1 "expected: ")
    (print expected)
    (print (equal actual expected))
    (equal actual expected)))

;;; Test cases:

(xt-deftest rmh-elfeed-org-remove-elfeed-tag
  (xt-note "The elfeed tag should not be assigned to the feeds")
  (xt-should (equal
              (rmh-elfeed-org-remove-tag '(("" elfeed tag1) ("" elfeed tag2)) 'elfeed)
              '(("" tag1) ("" tag2)))))

(xt-deftest rmh-elfeed-org-tagger-params
  (xt-note "Get paramemeters to create elfeed tagger")
  (xt-should (equal
              (rmh-elfeed-org-tagger-params '("entry-title:   hoi " tag0 tag1))
              '("hoi" (tag0 tag1)))))

(xt-deftest rmh-elfeed-org-trees-with-tag
  (xt-note "Use any number of trees tagged with \"elfeed\"")
  (xtd-should 'xt-trees-with-id-length
              ("fixture-no-ids-or-tags.org" 0)
              ("fixture-one-tag.org" 1)
              ("fixture-two-tags.org" 2)))

(xt-deftest rmh-elfeed-org-trees-with-id
  (xt-note "Use any number of trees with the id property \"elfeed\"")
  (xtd-should 'xt-trees-with-id-length
              ("fixture-no-ids-or-tags.org" 0)
              ("fixture-one-id.org" 1)
              ("fixture-two-ids.org" 2)))

(xt-deftest rmh-elfeed-org-headlines-from-tree
  (xt-note "Recusively include all feeds in a tree with their tags inherited from their parents")
  (xtd-should 'xt-feeds
              ("fixture-no-ids-or-tags.org" nil)
              ("fixture-one-tag.org"
               (("http1" elfeed tag1) ("http2" elfeed)))
              ("fixture-two-tags.org"
               (("http1" elfeed) ("http2" elfeed)))))

(xt-deftest rmh-elfeed-org-headliness-from-files
  (xt-note "Use all feeds in a multiple trees tagged with the \"elfeed\" tag and inherited their parent's tags")
  (xt-should (equal
              (rmh-elfeed-org-headliness-from-files '("fixture-one-tag.org" "fixture-two-ids.org") "elfeed" "http")
              '(("http1" tag1) ("http2") ("http1" tag0 tag1) ("http2" tag2)))))

(xt-deftest rmh-elfeed-org-headlines-and-entrytitles-from-files
  (xt-note "Use all feeds in a multiple trees tagged with the \"elfeed\" tag and inherited their parent's tags")
  (xt-should (equal
              (rmh-elfeed-org-headliness-from-files '("fixture-one-tag.org" "fixture-entry-title.org") "elfeed" "entry-title")
              '(("entry-title 1" tag1)))))

(xt-deftest rmh-elfeed-org-unique-headlines-and-entrytitles-from-files
  (xt-note "Should not return two \"http2\" entries")
  (xt-should (equal
              (rmh-elfeed-org-headliness-from-files '("fixture-one-tag.org" "fixture-entry-title.org") "elfeed" "\\(http\\|entry-title\\)")
              '(("http1" tag1) ("http2") ("entry-title 1" tag1)))))

(xt-deftest rmh-elfeed-org-feeds-get-from-with-none-found
  (xt-note "Make sure no nil values instead of feeds are returned")
  (xt-should (equal
              (rmh-elfeed-org-headliness-from-files '("fixture-one-tag.org" "fixture-one-id-no-feeds.org") "elfeed" "http")
              '(("http1" tag1) ("http2")))))


(provide 'elfeed-org-test)

;;; elfeed-org-test.el ends here
