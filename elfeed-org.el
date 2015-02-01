;;; elfeed-org.el --- Configure elfeed with one or more org-mode files

;; Copyright (C) 2014  Remy Honig

;; Author           : Remy Honig <remyhonig@gmail.com>
;; Package-Requires : ((elfeed "1.1.1") (org "8") (dash "2.10.0") (s "1.9.0"))
;; URL              : https://github.com/remyhonig/elfeed-org
;; Version          : 20150130.1
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
;; a lot.  Org-mode makes the book keeping of tags and feeds much
;; easier.  Tags get inherited from parent headlines.  Multiple files
;; can be specified to separate your private from your work feeds for
;; example.  You may also use tagging rules to tag feeds by entry-title
;; keywords.  See https://github.com/remyhonig/elfeed-org for usage.

;;; Code:

(require 'elfeed)
(require 'org)
(require 'dash)
(require 's)


(defgroup elfeed-org nil
  "Configure the Elfeed RSS reader with an Orgmode file"
  :group 'comm)


(defcustom rmh-elfeed-org-tree-id "elfeed"
  "The tag or ID property on the trees containing the RSS feeds."
  :group 'elfeed-org
  :type 'string)


(defcustom rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")
  "The files where we look to find trees with the `rmh-elfeed-org-tree-id'."
  :group 'elfeed-org
  :type '(repeat (file :tag "org-mode file")))


(defun rmh-elfeed-org-check-configuration-file (file)
  "Make sure FILE exists."
  (when (not (file-exists-p file))
    (error "Elfeed-org cannot open %s.  Make sure it exists customize the variable \'rmh-elfeed-org-files\'"
           (abbreviate-file-name file))))


(defun rmh-elfeed-org-import-trees (tree-id)
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


(defun rmh-elfeed-org-convert-tree-to-headlines (match tree)
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


(defun rmh-elfeed-org-cleanup-headlines (headlines tree-id)
  "In all HEADLINES given remove the TREE-ID."
  (mapcar (lambda (e) (delete tree-id e)) headlines))


(defun rmh-elfeed-org-import-headlines-from-files (files tree-id match)
  "Visit all FILES and return the headlines stored under tree tagged TREE-ID or with the \":ID:\" TREE-ID in one list, filter headlines on MATCH."
  (-distinct (-mapcat (lambda (file)
                        (with-current-buffer (find-file-noselect (expand-file-name file))
                          (org-mode)
                          (rmh-elfeed-org-cleanup-headlines
                           (rmh-elfeed-org-convert-tree-to-headlines match (rmh-elfeed-org-import-trees tree-id))
                           (intern tree-id))))
                      files)))


(defun rmh-elfeed-org-convert-headline-to-tagger-params (tagger-headline)
  "Add new entry hooks for tagging configured with the found headline in TAGGER-HEADLINE."
  (list
   (s-trim (s-chop-prefix "entry-title:" (car tagger-headline)))
   (cdr tagger-headline)))


(defun rmh-elfeed-org-export-entry-hook (tagger-params)
  "Export TAGGER-PARAMS to the proper `elfeed' structure."
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger
             :entry-title (car tagger-params)
             :add (cdr tagger-params))))


(defun rmh-elfeed-org-export-feed (headline)
  "Export HEADLINE to the proper `elfeed' structure."
  (add-to-list 'elfeed-feeds headline))


(defun rmh-elfeed-org-process (files tree-id)
  "Process headlines and taggers from FILES with org headlines with TREE-ID."

  ;; Warn if needed
  (-each files 'rmh-elfeed-org-check-configuration-file)
  
  ;; Clear elfeed structures
  (setq elfeed-feeds nil)
  (setq elfeed-new-entry-hook nil)

  ;; Concert org structure to elfeed structure
  (-each (rmh-elfeed-org-import-headlines-from-files files tree-id "\\(http\\|entry-title\\)")
    (lambda (headline)
      (let ((text (car headline)))
        (when (s-starts-with? "http" text)
          (rmh-elfeed-org-export-feed headline))
        (when (s-starts-with? "entry-title" text)
          (rmh-elfeed-org-export-entry-hook
           (rmh-elfeed-org-convert-headline-to-tagger-params headline))))))
  
  ;; Tell user what we did
  (message "elfeed-org loaded %i feeds, %i rules"
           (length elfeed-feeds)
           (length elfeed-new-entry-hook)))


;;;###autoload
(defun elfeed-org ()
  "Hook up rmh-elfeed-org to read the `org-mode' configuration when elfeed is run."
  (interactive)
  (message "elfeed-org is set up to handle elfeed configuration.")
  ;; Use an advice to load the configuration.
  (defadvice elfeed (before configure-elfeed activate)
    "Load all feed settings before elfeed is started."
    (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id)))


(provide 'elfeed-org)
;;; elfeed-org.el ends here
