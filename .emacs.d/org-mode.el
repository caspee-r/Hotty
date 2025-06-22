;; Org-Mode

 ;; Associate all org files with org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '(".*/[0-9]*-[0-9]*-[0-9]*$" . org-mode))
(add-to-list 'auto-mode-alist '("~/zettle/slip-box/[^.]*\\'" . org-mode))

(defun caspeer/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1)
    (setq
     ;; Edit settings
     org-auto-align-tags nil
     org-tags-column 0
	 ;; org-tag-alist '(
	 ;; 				 ("" . ?)
	 ;; 			 )
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     org-insert-heading-respect-content t
     org-use-speed-commands t
     org-return-follows-link t
     org-outline-path-complete-in-steps nil
     org-src-fontify-natively t
     org-src-tab-acts-natively t
     org-footnote-auto-adjust t
     org-log-done 'time
     org-log-into-drawer t
	 org-latex-image-default-scale "2"

     ;; TODO states

     ;; Org styling, hide markup etc.
     org-hide-emphasis-markers t
	 org-emphasis-alist '(("*" bold)
						  ("/" italic)
						  ("_" underline)
						  ("=" org-verbatim verbatim)
						  ("~" org-code verbatim)
						  ("+"
						   (:strike-through t))
						  ("!"
						   (:overline t)
						   verbatim))
     org-ellipsis " ▾"
     org-pretty-entities t
     ;; Agenda styling
     org-agenda-tags-column 0
     org-agenda-block-separator ?─
     org-agenda-start-with-log-mode t
     org-agenda-time-grid '((daily today require-timed)
							(800 1000 1200 1400 1600 1800 2000)
							" ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
     org-agenda-current-time-string
     "󰩓 now ─────────────────────────────────────────────────"

     org-agenda-custom-commands
     '(("d" "Dashboard"
		((agenda "" ((org-deadline-warning-days 7)))
         (todo "PLANNING"
               ((org-agenda-overriding-header "Next Tasks")))
         (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

       ("n" "Next Tasks"
		((todo "NEXT"
			   ((org-agenda-overriding-header "Next Tasks")))))
       )

     )

    (modify-all-frames-parameters
     '((right-divider-width . 40)
       (internal-border-width . 40)))
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel))
        (face-spec-reset-face face)
        (set-face-foreground face (face-attribute 'default :background)))
    (set-face-background 'fringe (face-attribute 'default :background))

    )

(defun caspeer/org-font-setup ()

    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
			    '(("^ *\\([-]\\) "
			       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
		    (org-level-2 . 1.1)
		    (org-level-3 . 1.05)
		    (org-level-4 . 1.0)
		    (org-level-5 . 1.1)
		    (org-level-6 . 1.1)
		    (org-level-7 . 1.1)
		    (org-level-8 . 1.1)))
	(set-face-attribute (car face) nil :font "ETBembo" :weight 'regular :height (cdr face)))

    (set-face-attribute 'org-block nil    :foreground "red" :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    ;(set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    ;(set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
	)


(defun caspeer/org-capture-inbox ()
	(interactive)
	(org-capture nil "i")
	)
(use-package org-modern
    :ensure t
	:config
	(setq org-modern-star 'replace)
	)

(use-package org
    :commands (org-capture org-agenda)
	:init
	(add-hook 'org-mode-hook #'caspeer/org-mode-setup)
	(add-hook 'org-mode-hook #'caspeer/org-font-setup)

	(add-hook 'org-mode-hook #'org-modern-mode)
    (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
	:bind (("C-c l" . org-store-link)
           ("C-c c" . org-capture)
		   ("C-c i" . 'caspeer/org-capture-inbox )
           ("C-M-|" . indent-rigidly)
		   ("C-c !" . org-timestamp)
		   )
    :config
    (global-set-key "\C-cb" 'org-switchb)
    (require 'org-habit)
	(require 'org-protocol)
	(add-to-list 'org-modules 'org-habit)
	(fringe-mode nil )
    (setq org-habit-graph-column 60)
    ;; Save Org buffers after refiling!
    (advice-add 'org-refile :after 'org-save-all-org-buffers)

    (setq org-capture-templates
		  `(
			("i" "Todo [INBOX] "
			 entry  (file+headline "~/org/inbox.org" "TASKS")
			 "* TODO %i%?"
			 )
			("y" "Youtube video" entry (file+headline "~/org/yt.org" "Watch Later")
			 "**  %?\n \n")

			("j" "Journal"
			 entry (function today-journal-file)
			 "* %<%Y-%m-%d>: \n %? "
			 :empty-lines 1
			 :clock-in t
			 :clock-resume t)

			("p" "Projects"
			 entry (file "~/org/Projects.org")
			 "** TODO %?\n"
			 :empty-lines 1
			 )
			("v" "English Vocabulary"
			 checkitem (file+headline "~/org/english.org" "Vocabulary" )
			 "[ ] =%?=  "
			 :empty-lines 1)
			))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "PLANNING(p)" "NEXT(n)" "|" "DONE(d!)" "Delegated(d)" )
            ))
	(setq org-refile-targets '(("~/org/someday.org" :level . 1)))
	(setq org-agenda-files (list "inbox.org"))
    ;; Configure custom agenda views
    (global-set-key "\C-ca" 'org-agenda)

    )

(use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
	;;:custom
    ;;(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
	)

(defun caspeer/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
    :ensure t
    :hook (org-mode . caspeer/org-mode-visual-fill))

(defvar org-electric-pairs '((?/ . ?/) (?= . ?=) ) "Electric pairs for org-mode.")
(defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'org-add-electric-pairs)

(defun caspeer/org-roam-zettle ()
		(interactive)
	(org-roam-capture nil "s")
	)

(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun caspeer/org-roam-dailies-name ()
	"Prompt for a title and create a filename based on it for daily notes."
	(let ((title (read-string "Title: ")))
		(concat (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title) ".org"))
	)


;;Org-Roam

(defcustom caspeer/org-roam-filter-entries '("refs" "slip-box")
	"org-roam entries to filter with"
	:type '(repeat string)
	:group 'org-roam
	)

(defun caspeer/org-roam-node--include (node folders)
	"include only org-roam nodes in the specified folders"
	(let ((node-path (org-roam-node-file node)))
		(or (member (f-base (f-parent node-path)) folders) (member node-path folders))
		))

(defun caspeer/org-roam-node-find-entry (&optional new-entries)
	"find only nodes within the provided entries or in org-roam-filter-entires"
	(interactive)
	(let ((selected-entries (completing-read-multiple "select entries (separated by ','):" caspeer/org-roam-filter-entries)))
		 (org-roam-node-find nil nil (lambda (node) (caspeer/org-roam-node--include node selected-entries)))
		 )
	)


(use-package org-roam
	:ensure t
	:bind (
		   ("C-c n r" . org-roam-node-random)
		   ("C-c n f" . org-roam-node-find)
		   ("C-c n e" . caspeer/org-roam-node-find-entry)
		   ("C-c n c" . org-roam-capture)
		   ("C-c n z" . caspeer/org-roam-zettle)
		   ("C-c n t" . org-roam-dailies-capture-today)
		   (:map org-mode-map
				 ("C-c n g" . org-roam-graph)
				 ("C-c n l" . org-roam-buffer-toggle)
				 ("C-c n t" . org-id-get-create)
				 ("C-c n a" . org-roam-aliase-add)
				 ("C-c n i" . org-roam-node-insert)
				 ("C-c n v" . #'org-roam-node-visit)
				 ;; Dailies
				 ;; ("C-c j" . org-roam-dailies-capture-today)
				 ;; ("C-c d" . org-roam-dailies-goto-today)
				 ))
										;:bind-keymap
;("C-c n d" . org-roam-dailies-map)
	:custom
	(org-roam-directory (file-truename "~/zettle"))
	(roam-completion-everywhere nil)
	(org-roam-capture-templates
	 '(
	   ("s" "slip-box" plain "%?"
		:target (file+head "slip-box/${slug}.org"
						   "#+title:"
						   )
		:unnarrowed t
		:unnarrowed-sections (1))
	   ("r" "refrence" plain "%?"
		:target (file+head "refs/${slug}.org" "#+title:${title}\n
#+date:%<%Y-%m-%d %a>")
		:unnarrowed t
		)
	   ))
	;; Add more templates as needed

	:config
	(require 'org-roam-dailies)
	(cl-defmethod org-roam-node-my-title ((node org-roam-node))
		(let ((node-dir (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
			(if (string= node-dir "slip-box/")
					(concat (org-roam-node-title node) " : " (f-base (org-roam-node-file node)))
				(org-roam-node-title node))
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
		  org-roam-dailies-capture-templates '(( "d" "default" entry
												 "* %<%H:%M>: %?"
												:if-new (file+head "%<%Y-%m-%d>.org"
														 "#+title: %<%Y-%m-%d>\n")
												)
											   ( "f" "fleeting note" entry "* %<%Y-%m-%d> at %<%H:%M> %? %(ignore-errors (org-id-get-create))"
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
