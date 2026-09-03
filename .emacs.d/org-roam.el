;;Org-Roam

(defcustom caspeer/org-roam-filter-entries '("notes" "journal")
	"org-roam entries to filter with"
	:type '(repeat string)
	:group 'org-roam
	)

(defun caspeer/org-roam-node--include-folder (node folders)
	"include only org-roam nodes in the specified folders"
	(let ((node-path (org-roam-node-file node)))
		(or (member (f-base (f-parent node-path)) folders) (member node-path folders))
		))

(defun caspeer/org-roam-node--include-tag (node tags)
	;;NOTE(caspeer): this filter only by the first tag in the node
	"include only org-roam nodes which has the specified tags"
	(let ((node-tags (org-roam-node-tags node)))
		(member (nth 0 node-tags) tags)
		))

(defun caspeer/org-roam-node-find-entry (&optional entries)
	"find only nodes within the provided entries or in org-roam-filter-entires"
	(interactive)
	(or entries (setq entries caspeer/org-roam-filter-entries))
	(if (length> entries 1)
			(let ((selected-entries (completing-read-multiple "select entries (separated by ','):" entries)))
				(org-roam-node-find nil nil (lambda (node) (caspeer/org-roam-node--include-folder node selected-entries)))
				)
		(org-roam-node-find nil nil (lambda (node) (caspeer/org-roam-node--include-folder node entries)))
		)
	)

(defun caspeer/org-roam-node-find-by-tag ()
	""
	(interactive)
	(let ((selected (completing-read-multiple "select tags (separated by ','):"
											  (-filter (lambda (tag) (not (s-prefix? "@" tag))) (org-roam-tag-completions)))
					))
		(org-roam-node-find nil nil (lambda (node) (caspeer/org-roam-node--include-tag node selected)))
		)
	)




(use-package org-roam
	:ensure t
	:bind (
		   ("C-c n r" . org-roam-node-random)
		   ("C-c n f" . org-roam-node-find)
		   ("C-c n e" . caspeer/org-roam-node-find-entry)
		   ("C-c n t" . caspeer/org-roam-node-find-by-tag)
		   ("C-c n c" . org-roam-capture)
		   ("C-c n d" . org-roam-dailies-capture-today)
		   (:map org-mode-map
				 ("C-c n g" . org-roam-graph)
				 ("C-c n l" . org-roam-buffer-toggle)
				 ("C-c n a" . org-roam-aliase-add)
				 ("C-c n i" . org-roam-node-insert)
				 ("C-c n v" . #'org-roam-node-visit)
				 ;; Dailies
				 ;; ("C-c j" . org-roam-dailies-capture-today)
				 ;; ("C-c d" . org-roam-dailies-goto-today)
				 ))
										;:bind-keymap
	:custom
	(org-roam-directory (file-truename "~/notes"))
	(roam-completion-everywhere nil)
	(org-roam-capture-templates
	 '(
	   ("n" "notes" plain "%?"
		:target (file+head "${slug}.org"
						   "#+title:\n#+date:%T"
						   )
		:unnarrowed t
		:unnarrowed-sections (1))
	   ("i" "Inbox idea"
		plain
		"%?"
		:target (file+head "INBOX.org"
						   "#+title: Inbox\n")
		:unnarrowed t)

	   ))
	;; Add more templates as needed

	:config
	(require 'org-roam-dailies)
	(cl-defmethod org-roam-node-my-title ((node org-roam-node))
		(let ((node-dir (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
			(concat (org-roam-node-title node) " : " (f-base (org-roam-node-file node)))
			)
		)
	(cl-defmethod org-roam-node-directories ((node org-roam-node))
		"Access slot \"directory\" of org-roam-node struct CL-X"
		(if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
				(format "(%s)" (car (f-split dirs)))
			""))

	(add-to-list 'display-buffer-alist
				 '("\\*org-roam\\*"
				   (display-buffer-in-direction)
				   (direction . right)
				   (window-width . 0.40)
				   (window-height . fit-window-to-buffer)))
	(org-roam-db-autosync-mode)
	(setq org-roam-file-extensions '("org" "org_archive")
		  org-roam-dailies-directory "journal/"
		  org-roam-completion-everywhere t
		  org-id-extra-files (org-roam-list-files)
		  org-roam-dailies-capture-templates '(( "j" "journal bullet point " entry
												 "* %U %?"
												 :if-new (file+head "%<%Y-%m-%d>.org"
																	"#+title: %<%Y-%m-%d>\n")
												 )
											   ("f" "fleeting note" entry "* %<%Y-%m-%d> at %<%H:%M> %? %(ignore-errors (org-id-get-create))"
												:if-new (file+head "inbox.org" "* Fleeting Notes")
												)
											   )
		  org-roam-node-display-template "${my-title}"
		  org-roam-capture-ref-templates
		  '(("r" "ref" entry "* %?" :target
			 (file+head "refs/${slug}.org" "#+title: ${title}\n
#+date: %<%Y-%m-%d> %a\n
#+filetags:")
			 :unnarrowed t
			 ))
		  )
	(require 'org-roam-protocol)	;; If using org-roam-protocol
	)
