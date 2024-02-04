;; Org-Mode

 ;; Associate all org files with org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '(".*/[0-9]*-[0-9]*-[0-9]*$" . org-mode))
(add-to-list 'auto-mode-alist '("~/org/zettle/slip-box/[^.]*\\'" . org-mode))

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
     "󱫙 now ─────────────────────────────────────────────────"

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
	(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

    (set-face-attribute 'org-block nil    :foreground "red" :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))


(defun caspeer/org-capture-inbox ()
	(interactive)
	(org-capture nil "i")
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
           ("C-M-|" . indent-rigidly))
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
			("a" "Assignment" entry (file+headline "~/org/college.org" "Assignments")
			 "* TODO %?\n %^g \n  SCHEDULED: %t\n  :PROPERTIES:\n  :DEADLINE: %^T\n  :END:")

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
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))




(defun caspeer/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
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


(defcustom org-roam-filter-by-entries '("refs")
  "Entries (tags or directories) to be excluded for Org-roam node filtering."
  :type '(repeat string)
  :group 'org-roam)



;; Helper function to filter org-roam-node by entries (tags or directories).
(defun caspeer/org-roam-node--filter-by-entries (node &optional included-entries excluded-entries)
  "Filter org-roam-node by entries (tags or directories)."
  (let* ((entries (append (org-roam-node-tags node)
                          (butlast (f-split (f-relative (org-roam-node-file node) org-roam-directory)))))
         (included (and included-entries (not (seq-some (lambda (entry) (member entry entries)) included-entries))))
         (excluded (and excluded-entries (seq-some (lambda (entry) (member entry entries)) excluded-entries))))
    (if (or included excluded)
        nil t)))

;; Helper function to filter org-roam-node to show only nodes with entries in `org-roam-filter-by-entries`.
(defun caspeer/org-roam-node--filter-excluded (node)
  "Filter org-roam-node to show only nodes with entries in `org-roam-filter-by-entries`."
  (caspeer/org-roam-node--filter-by-entries node nil org-roam-filter-by-entries))


;; Modded org-roam-node-find which filters nodes using entries (tags or directories).
(defun caspeer/org-roam-node-find ()
  "Show Org-roam nodes, filtering by entries (tags or directories)."
  (interactive)
  (org-roam-node-find nil nil
                      (lambda (node) (caspeer/org-roam-node--filter-by-entries node nil org-roam-filter-by-entries))))

;; Show only Org-roam nodes that match entries in `org-roam-filter-by-entries`.
(defun caspeer/org-roam-node-find-only ()
	"Show only Org-roam nodes that match entries in `org-roam-filter-by-entries`."
	(interactive)
	(org-roam-node-find nil nil (lambda (node) (not (caspeer/org-roam-node--filter-excluded node)))))

(defun caspeer/org-roam-node-find-entry (&optional new-entries)
  "Show only Org-roam nodes matching the specified entries interactively.
If NEW-ENTRIES are provided, use them as the entries to match by (seperate entries by ,).
Otherwise, prompt the user to select from existing entries."
  (interactive)
  (if new-entries
      (let ((org-roam-filter-by-entries new-entries))
        (caspeer/org-roam-node-find-only))
    (let ((selected-entries (completing-read-multiple "Select entries to filter by (seperate by ,): " org-roam-filter-by-entries)))
      (let ((org-roam-filter-by-entries selected-entries))
        (caspeer/org-roam-node-find-only)))))


(define-skeleton fleeting-note-skeleton
	"my fleeting notes skeleton definition"
	"* ${title}"
	)

(defun caspeer/org-roam-init-fleeting-note ()
	""
	(interactive)
	(concat "** ${title} %?")
	(org-id-get-create)
	(org-todo)
	(org-newline-and-indent)

	)

;;Org-Roam
(use-package org-roam
	:ensure t
	;; :after org
	:bind (
		   ("C-c n r" . org-roam-node-random)
		   ("C-c n f" . org-roam-node-find)
		   ("C-c n c" . org-roam-capture)
		   ("C-c n z" . caspeer/org-roam-zettle)
		   ("C-c n i" . org-roam-dailies-capture-today)
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
				 ))						;
	:custom
	(org-roam-directory (file-truename "~/org/zettle"))
	(roam-completion-everywhere t)
	(org-roam-capture-templates
	 '(
	   ("s" "slip-box" plain "%?"
		:target (file+head "slip-box/${slug}"
						   "#+title:"
						   )
		:unnarrowed t
		:unnarrowed-sections (1))
	   ("r" "refrence" plain "%?"
		:target (file+head "refs/${slug}.org" "#+title:${title}\n
#+created:%<%Y-%m-%d>\n
")
		:unnarrowed t
		)
	   	   ))
	;; Add more templates as needed
	:config
	
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
		  org-id-extra-files (org-roam-list-files)
		  org-roam-dailies-directory "journal/"
		  org-roam-dailies-capture-templates '(
											   ("f" "fleeting" entry
												"** TODO %i%?"
												:target (file+head "inbox.org" "#+title: %<%Y-%m-%d>\n
* Fleeting Thoughts\n")
												)
											   )
		  org-roam-node-display-template "${title}"
		  org-roam-capture-ref-templates 
		  '(("r" "ref" entry "* %?" :target
			 (file+head "refs/${slug}.org" "#+title: ${title}\n
#+Date: %<%Y-%m-%d>\n
#+filetags:\n")
			 :unnarrowed t
			  ))
		  )
		  
	;; If using org-roam-protocol
	(require 'org-roam-protocol))




(use-package org-pomodoro
	:ensure t
	:bind
	(
	 ("C-c t s" . org-pomodoro)
	 )
	:config

	(setq
	 org-pomodoro-length 55
	 org-pomodoro-short-break-length 5
	 org-pomodoro-long-break-length 15
	 org-pomodoro-long-break-frequency 2
	 org-pomodoro-manual-break t
	 org-pomodoro-play-sounds t
	 org-pomodoro-ask-upon-killing t
	 org-pomodoro-ticking-frequency 2
	 org-pomodoro-audio-player "mpv"
	 org-pomodoro-clock-break t
	 org-pomodoro-expiry-time 150
	 )
	)

(defun pomodoro-kill ()
	"interactive org-pomodoro-kill"
	(interactive)
	(org-pomodoro-kill)
	)

(global-set-key (kbd "C-c t k") 'pomodoro-kill)
	;; (setq org-roam-capture-templates
	;;           '(("d" "default" plain
	;; 			 (function org-roam--capture-get-point)
	;; 			 "%?"
	;; 			 :file-name "${slug}"
	;; 			 :head "#+title: ${title}\n"
	;; 			 :unnarrowed t)

	;; 			("s" "slip-box" plain
	;; 			 (function org-roam--capture-get-point)
	;; 			 "%?"
	;; 			 :file-name "slip-box/${slug}"
	;; 			 :head "#+title: ${title}\n"
	;; 			 :unnarrowed t
	;; 			 :unnarrowed-sections (1))

	;; 			;; Add more templates as needed
	;; 			))


	;; Journaling

(defvar org-journal-dir "~/journal/")
(defun get-today-journal-file ()
  (let ((daily-name (format-time-string "%Y-%m-%d")))
       (expand-file-name (concat org-journal-dir daily-name)))
  )

(defun today-journal-file ()
    "Create and load a journal file based on today's date."
    (interactive)
    (set-buffer (org-capture-target-buffer (get-today-journal-file)))
    (goto-char (point-max))
    )

;; GTD




