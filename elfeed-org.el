;;; elfeed-org.el --- Configure elfeed with one or more org-mode files

;; Copyright (C) 2014  Remy Honig

;; Author           : Remy Honig <remyhonig@gmail.com>
;; Package-Requires : ((elfeed "1.1.1") (org "8.2.7") (dash "2.10.0") (s "1.9.0") (cl-lib "0.5"))
;; URL              : https://github.com/remyhonig/elfeed-org
;; Version          : 20170422.1
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
(require 'cl-lib)
(require 'xml)


(defgroup elfeed-org nil
  "Configure the Elfeed RSS reader with an Orgmode file"
  :group 'comm)


(defcustom rmh-elfeed-org-tree-id "elfeed"
  "The tag or ID property on the trees containing the RSS feeds."
  :group 'elfeed-org
  :type 'string)

(defcustom rmh-elfeed-org-ignore-tag "ignore"
  "The tag on the feed trees that will be ignored."
  :group 'elfeed-org
  :type 'string)

(defcustom rmh-elfeed-org-auto-ignore-invalid-feeds nil
  "If non-nil, elfeed-org will mark the invalid feeds ignored
  after fetch operation."
  :group 'elfeed-org
  :type 'bool)

(defcustom rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")
  "The files where we look to find trees with the `rmh-elfeed-org-tree-id'."
  :group 'elfeed-org
  :type '(repeat (file :tag "org-mode file")))


(defun rmh-elfeed-org-check-configuration-file (file)
  "Make sure FILE exists."
  (when (not (file-exists-p file))
    (error "Elfeed-org cannot open %s.  Make sure it exists customize the variable \'rmh-elfeed-org-files\'"
           (abbreviate-file-name file))))

(defun rmh-elfeed-org-is-headline-contained-in-elfeed-tree ()
  "Is any ancestor a headline with the elfeed tree id.
Return t if it does or nil if it does not."
  (let ((result nil))
    (save-excursion
      (while (and (not result) (org-up-heading-safe))
        (setq result (member rmh-elfeed-org-tree-id (org-get-tags))))
    result)))

(defun rmh-elfeed-org-mark-feed-ignore (url)
  "Set tag `rmh-elfeed-org-ignore-tag' to headlines containing
the feed url."
  (dolist (org-file rmh-elfeed-org-files)
    (with-current-buffer (find-file-noselect
                          (expand-file-name org-file))
      (org-mode)
      (beginning-of-buffer)
      (while (and
              (search-forward url nil t)
              (org-on-heading-p)
              (rmh-elfeed-org-is-headline-contained-in-elfeed-tree))
        (org-toggle-tag rmh-elfeed-org-ignore-tag 'on))
      (elfeed-log 'info "elfeed-org tagged '%s' in file '%s' with '%s' to be ignored" url org-file rmh-elfeed-org-ignore-tag))))


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


(defun rmh-elfeed-org-convert-tree-to-headlines (parsed-org)
  "Get the inherited tags from PARSED-ORG structure if MATCH-FUNC is t.
The algorithm to gather inherited tags depends on the tree being
visited depth first by `org-element-map'.  The reason I don't use
`org-get-tags-at' for this is that I can reuse the parsed org
structure and I am not dependent on the setting of
`org-use-tag-inheritance' or an org buffer being present at
all.  Which in my opinion makes the process more traceable."
  (let* ((tags '())
         (level 1))
    (org-element-map parsed-org 'headline
      (lambda (h)
        (let* ((current-level (org-element-property :level h))
               (delta-level (- current-level level))
               (delta-tags (mapcar 'intern (org-element-property :tags h)))
               (heading (org-element-property :raw-value h)))
          ;; update the tags stack when we visit a parent or sibling
          (unless (> delta-level 0)
            (let ((drop-num (+ 1 (- delta-level))))
              (setq tags (-drop drop-num tags))))
          ;; save current level to compare with next heading that will be visited
          (setq level current-level)
          ;; save the tags that might apply to potential children of the current heading
          (push (-concat (-first-item tags) delta-tags) tags)
          ;; return the heading and inherited tags
          (-concat (list heading)
                   (-first-item tags)))))))

;; TODO: mark wrongly formatted feeds (PoC for unretrievable feeds)
(defun rmh-elfeed-org-flag-headlines (parsed-org)
  "Flag headlines in PARSED-ORG if they don't have a valid value."
  (org-element-map parsed-org 'headline
    (lambda (h)
      (let ((tags (org-element-property :tags h)))
        (org-element-put-property h :tags (push "_flag_" tags))))))


(defun rmh-elfeed-org-filter-relevant (list)
  "Filter relevant entries from the LIST."
  (-filter
   (lambda (entry)
     (and
      (string-match "\\(http\\|entry-title\\)" (car entry))
      (not (member (intern rmh-elfeed-org-ignore-tag) entry))))
   list))


(defun rmh-elfeed-org-cleanup-headlines (headlines tree-id)
  "In all HEADLINES given remove the TREE-ID."
  (mapcar (lambda (e) (delete tree-id e)) headlines))


(defun rmh-elfeed-org-import-headlines-from-files (files tree-id)
  "Visit all FILES and return the headlines stored under tree tagged TREE-ID or with the \":ID:\" TREE-ID in one list."
  (-distinct (-mapcat (lambda (file)
                        (with-current-buffer (find-file-noselect (expand-file-name file))
                          (org-mode)
                          (rmh-elfeed-org-cleanup-headlines
                           (rmh-elfeed-org-filter-relevant
                            (rmh-elfeed-org-convert-tree-to-headlines
                             (rmh-elfeed-org-import-trees tree-id)))
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
             :entry-title (nth 0 tagger-params)
             :add (nth 1 tagger-params))))

(defun rmh-elfeed-org-export-feed (headline)
  "Export HEADLINE to the proper `elfeed' structure."
  (if (and (stringp (car (last headline)))
           (> (length headline) 1))
      (progn
        (add-to-list 'elfeed-feeds (butlast headline))
        (let ((feed (elfeed-db-get-feed (car headline))))
          (setf (elfeed-meta feed :title) (car (last headline)))
          (elfeed-meta feed :title)))
    (add-to-list 'elfeed-feeds headline)))

(defun rmh-elfeed-org-process (files tree-id)
  "Process headlines and taggers from FILES with org headlines with TREE-ID."

  ;; Warn if configuration files are missing
  (-each files 'rmh-elfeed-org-check-configuration-file)

  ;; Clear elfeed structures
  (setq elfeed-feeds nil)
  (setq elfeed-new-entry-hook nil)

  ;; Convert org structure to elfeed structure and register taggers and subscriptions
  (let* ((headlines (rmh-elfeed-org-import-headlines-from-files files tree-id))
         (subscriptions (rmh-elfeed-org-filter-subscriptions headlines))
         (taggers (rmh-elfeed-org-filter-taggers headlines))
         (elfeed-taggers (-map 'rmh-elfeed-org-convert-headline-to-tagger-params taggers))
         (elfeed-tagger-hooks (-map 'rmh-elfeed-org-export-entry-hook elfeed-taggers)))
    (-each subscriptions 'rmh-elfeed-org-export-feed)
    (-each taggers 'rmh-elfeed-org-export-entry-hook))

  ;; Tell user what we did
  (elfeed-log 'info "elfeed-org loaded %i feeds, %i rules"
           (length elfeed-feeds)
           (length elfeed-new-entry-hook)))


(defun rmh-elfeed-org-filter-taggers (headlines)
  "Filter tagging rules from the HEADLINES in the tree."
  (-non-nil (-map
             (lambda (headline)
               (when (s-starts-with? "entry-title" (car headline)) headline))
             headlines)))

(defun rmh-elfeed-org-filter-subscriptions (headlines)
  "Filter subscriptions to rss feeds from the HEADLINES in the tree."
  (-non-nil (-map
             (lambda (headline)
               (let* ((text (car headline))
                      (link-and-title (s-match "^\\[\\[\\(http.+?\\)\\]\\[\\(.+?\\)\\]\\]" text))
                      (hyperlink (s-match "^\\[\\[\\(http.+?\\)\\]\\(?:\\[.+?\\]\\)?\\]" text)))
                 (cond ((s-starts-with? "http" text) headline)
                       (link-and-title (-concat (list (nth 1 hyperlink))
                                                (cdr headline)
                                                (list (nth 2 link-and-title))))
                       (hyperlink (-concat (list (nth 1 hyperlink)) (cdr headline))))))
             headlines)))


(defun rmh-elfeed-org-convert-opml-to-org (xml level)
  "Convert OPML content to Org format."
  (cl-loop for (tag attr . content) in (cl-remove-if-not #'listp xml)
           when (and (not (assoc 'xmlUrl attr)) (assoc 'title attr))
           concat (format "%s %s\n" (make-string level ?*) (cdr it))
           when (assoc 'xmlUrl attr)
           concat (format "%s [[%s][%s]]\n" (make-string level ?*)
                          (cdr it) (cdr (assoc 'title attr)))
           concat (rmh-elfeed-org-convert-opml-to-org content (+ 1 level))))

(defun elfeed-org-import-opml (opml-file)
  "Import feeds from OPML file to a temporary Org buffer."
  (interactive "FInput OPML file: ")
  (let* ((xml (xml-parse-file opml-file))
        (content (rmh-elfeed-org-convert-opml-to-org xml 0)))
    (with-current-buffer (get-buffer-create "*Imported Org Feeds*")
      (erase-buffer)
      (insert (format "* Imported Feeds            :%s:\n" rmh-elfeed-org-tree-id))
      (insert content)
      (org-mode)
      (pop-to-buffer (current-buffer)))))


(defun rmh-elfeed-org-convert-org-to-opml (org-buffer)
  "Convert Org buffer content to OPML format."
  (let (need-ends
        opml-body)
    (with-current-buffer org-buffer
      (org-mode)
      (org-element-map (rmh-elfeed-org-import-trees
                        rmh-elfeed-org-tree-id) 'headline
        (lambda (h)
          (let* ((current-level (org-element-property :level h))
                 (tags (org-element-property :tags h))
                 (heading (org-element-property :raw-value h))
                 (link-and-title (s-match "^\\[\\[\\(http.+?\\)\\]\\[\\(.+?\\)\\]\\]" heading))
                 (hyperlink (s-match "^\\[\\[\\(http.+?\\)\\]\\(?:\\[.+?\\]\\)?\\]" heading))
                 url
                 title
                 opml-outline)
            ;; fill missing end outlines
            (while (and (car need-ends) (>= (car need-ends) current-level))
              (let* ((level (pop need-ends)))
                (setq opml-body (concat opml-body (format "  %s</outline>\n"
                                                          (make-string (* 2 level) ? ))))))

            (cond ((s-starts-with? "http" heading)
                   (setq url heading)
                   (setq title "Unknown"))
                  (link-and-title (setq url (nth 1 link-and-title))
                                  (setq title (nth 2 link-and-title)))
                  (hyperlink (setq url (nth 1 hyperlink))
                             (setq title "Unknown"))
                  (t (setq title heading)))
            (if url
                (setq opml-outline (format "  %s<outline title=\"%s\" xmlUrl=\"%s\"/>\n"
                                           (make-string (* 2 current-level) ? )
                                           title (xml-escape-string url)))
              (unless (s-starts-with? "entry-title" heading)
                (unless (member rmh-elfeed-org-tree-id tags)
                  ;; insert category title only when it is neither the top
                  ;; level elfeed node nor the entry-title node
                  (progn
                    (push current-level need-ends)
                    (setq opml-outline (format "  %s<outline title=\"%s\">\n"
                                               (make-string (* 2 current-level) ? )
                                               title))))))
            (setq opml-body (concat opml-body opml-outline))))))

    ;; fill missing end outlines at end
    (while (car need-ends)
      (let* ((level (pop need-ends)))
        (setq opml-body (concat opml-body (format "  %s</outline>\n"
                                                  (make-string (* 2 level) ? ))))))
    opml-body))

(defun elfeed-org-export-opml ()
  "Export Org feeds under `rmh-elfeed-org-files' to a temporary
  OPML buffer with tree structures. Note that the first level
  elfeed node will be ignored if there is not url for it, and
  user may need edit the output again because most of Feed/RSS
  readers only support maximize 2 level tree nodes."
  (interactive)
  (let ((opml-body (cl-loop for org-file in rmh-elfeed-org-files
                             concat (rmh-elfeed-org-convert-org-to-opml
                                     (find-file-noselect (expand-file-name org-file))))))
    (with-current-buffer (get-buffer-create "*Exported OPML Feeds*")
      (erase-buffer)
      (insert "<?xml version=\"1.0\"?>\n")
      (insert "<opml version=\"1.0\">\n")
      (insert "  <head>\n")
      (insert "    <title>Elfeed-Org Export</title>\n")
      (insert "  </head>\n")
      (insert "  <body>\n")
      (insert opml-body)
      (insert "  </body>\n")
      (insert "</opml>\n")
      (xml-mode)
      (pop-to-buffer (current-buffer)))))


;;;###autoload
(defun elfeed-org ()
  "Hook up rmh-elfeed-org to read the `org-mode' configuration when elfeed is run."
  (interactive)
  (elfeed-log 'info "elfeed-org is set up to handle elfeed configuration")
  ;; Use an advice to load the configuration.
  (defadvice elfeed (before configure-elfeed activate)
    "Load all feed settings before elfeed is started."
    (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id))
  (add-hook 'elfeed-http-error-hooks (lambda (url status)
                                       (when rmh-elfeed-org-auto-ignore-invalid-feeds
                                         (rmh-elfeed-org-mark-feed-ignore url))))
  (add-hook 'elfeed-parse-error-hooks (lambda (url error)
                                        (when rmh-elfeed-org-auto-ignore-invalid-feeds
                                          (rmh-elfeed-org-mark-feed-ignore url)))))


(provide 'elfeed-org)
;;; elfeed-org.el ends here
