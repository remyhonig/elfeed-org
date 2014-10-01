(require 'cl)
(require 'elfeed)

(defvar rmh-elfeed-org-tree-id "elfeed"
  "The ID property of the tree containing the RSS feeds")

(defvar rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")
  "The files where we look for to find the tree with the rmh-elfeed-org-tree-id")

(defun rmh-elfeed-org-read-tree (tree-id match)
  "Convert an org tree into an Elfeed RSS feeds configuration compatible structure. Filter out headlines that contain MATCH"
  (let ((m (org-id-find tree-id 'marker)))
    (save-excursion
      (set-buffer (marker-buffer m))
      (with-current-buffer
	  (marker-buffer m)
	(progn
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
	       nil rmh-elfeed-org-files)))))))))

(defun rmh-elfeed-org-tags-inherited (func)
  "Call FUNC while ensuring tags are inherited"
  (let ((original org-use-tag-inheritance))
    (progn (setq org-use-tag-inheritance 't)
           (let ((feeds (funcall func)))
             (setq org-use-tag-inheritance original) feeds))))

(defun rmh-elfeed-org-add-new-entry-hooks (keywords)
  "Add new entry hooks for tagging keywords"
  (mapcar
   (lambda (x)
     (progn
            (let* ((term (car (cdr (split-string (car x) ": ")))) (tags (cdr x)))
              (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-title term :add (cdr x)))))) keywords))

(defun rmh-elfeed-org-check-configuration-file (file)
  "Make sure FILE exists. If not, ask user what to do."
  (when (not (file-exists-p file))
    (error "rmh-elfeed-org cannot open %s. Make sure it exists or set the variable \'rmh-elfeed-org-files\'"
           (abbreviate-file-name file))
    ))

(defun rmh-elfeed-org-configure ()
  "Clear and reload the Elfeed feeds- and tagging configuration"
  (progn (mapcar (lambda (file) (rmh-elfeed-org-check-configuration-file file)) rmh-elfeed-org-files)
         (let ((keywords (rmh-elfeed-org-read-tree rmh-elfeed-org-tree-id "entry-title"))
               (feeds (rmh-elfeed-org-read-tree rmh-elfeed-org-tree-id "http")))
           (progn
             (setq elfeed-new-entry-hook nil)
             (setq elfeed-feeds feeds)
             (rmh-elfeed-org-add-new-entry-hooks keywords)))))

(provide 'rmh-elfeed-org)
