;;; elfeed-org.el --- Configure elfeed with one or more org-mode files

;; Copyright (C) 2014  Remy Honig

;; Author           : Remy Honig <remyhonig@gmail.com>
;; Package-Requires : ((elfeed "1.1.1") (org "7"))
;; URL              : https://github.com/remyhonig/elfeed-org
;; Version          : 20141003.1
;; Keywords         : news

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
;; Maintaining tags for all rss feeds is cumbersome using the regular
;; flat list where there is no hierarchy and tag names are duplicated
;; a lot. Org-mode makes the book keeping of tags and feeds much
;; easier. Tags get inherited from parent headlines. Multiple files
;; can be specified to separate your private from your work feeds for
;; example. You may also use tagging rules to tag feeds by entry-title
;; keywords.

;;; Code:

(require 'elfeed)
(require 'cl)
(require 'org)


(defvar rmh-elfeed-org-tree-id "elfeed"
  "The ID property of the tree containing the RSS feeds.")


(defvar rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")
  "The files where we look to find the tree with the `rmh-elfeed-org-tree-id'.")


(defun rmh-elfeed-org-read-tree (tree-id match)
  "Convert org tree with TREE-ID into a feed configuration structure for Elfeed.
Filter out headlines that contain MATCH."
  (let* ((m (org-id-find tree-id 'marker))
         (buf (marker-buffer m)))
    (save-excursion
      (with-current-buffer buf
        (goto-char m)
        (move-marker m nil)
        (remove-if-not
         (lambda (x)
           (and (string-match match (car x)) x))
         (rmh-elfeed-org-tags-inherited
          (lambda ()
            (org-map-entries
             '(let ((url (substring-no-properties (org-get-heading t)))
                    (tags (mapcar 'intern (org-get-tags-at))))
                (append (list url) tags))
             nil
             rmh-elfeed-org-files))))))))


(defun rmh-elfeed-org-tags-inherited (func)
  "Call FUNC while ensuring tags are inherited."
  (let ((original org-use-tag-inheritance))
    (setq org-use-tag-inheritance 't)
    (let ((feeds (funcall func)))
      (setq org-use-tag-inheritance original) feeds)))


(defun rmh-elfeed-org-add-new-entry-hooks (keywords)
  "Add new entry hooks for tagging KEYWORDS."
  (mapc
   (lambda (x)
     (let ((term (car (cdr (split-string (car x) ": "))))
           (tags (cdr x)))
       (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-title term :add (cdr x)))))
   keywords))


(defun rmh-elfeed-org-check-configuration-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
  (when (not (file-exists-p file))
    (error "Elfeed-org cannot open %s.  Make sure it exists or set the variable \'rmh-elfeed-org-files\'"
           (abbreviate-file-name file))))


(defun rmh-elfeed-org-configure ()
  "Clear and reload the elfeed feeds- and tagging configuration."

  ;; To regenerate the elfeed config after the org mode tree has
  ;; changed we want this function to be interactive
  (interactive)

  ;; Make sure all files really exists
  (mapc (lambda (file) (rmh-elfeed-org-check-configuration-file file)) rmh-elfeed-org-files)

  ;; Clear hooks for auto tagging
  (setq elfeed-new-entry-hook (list))

  ;; Clear registered feeds
  (setq elfeed-feeds (list))

  ;; Add feeds and auto tagging rules to elfeed configuration
  (mapc (lambda (file) (with-current-buffer (find-file-noselect file)

                    ;; Extract tagging rules and feeds from the found file
                    (let* ((tree-id rmh-elfeed-org-tree-id)
                           (tagging-rules (rmh-elfeed-org-read-tree tree-id "entry-title"))
                           (feeds (rmh-elfeed-org-read-tree tree-id "http")))

                      ;; Add feeds to elfeed configuration
                      (setq elfeed-feeds (append elfeed-feeds feeds))

                      ;; Add auto tagging rules to elfeed configuration
                      (rmh-elfeed-org-add-new-entry-hooks tagging-rules))))

        ;; The files we are operating on
        rmh-elfeed-org-files)

  ;; Tell user what we did
  (message "elfeed-org loaded %i feeds, %i rules"
           (length elfeed-feeds)
           (length elfeed-new-entry-hook)))


;;;###autoload
(defun elfeed-org ()
  "Hook up rmh-elfeed-org to read the `org-mode' configuration when elfeed starts."
  (interactive)
  (message "elfeed-org is set up to handle elfeed configuration.")
  ;; Use an advice to load the configuration.
  (defadvice elfeed (before configure-elfeed activate)
    "Load all feed settings before elfeed is started."
    (rmh-elfeed-org-configure)))


(provide 'elfeed-org)
;;; elfeed-org.el ends here
