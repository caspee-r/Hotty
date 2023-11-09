;; Org-Mode 

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '(".*/[0-9]*-[0-9]*-[0-9]*$" . org-mode))
(defun caspeer/org-mode-setup ()
    (org-indent-mode)
    ;; Associate all org files with org mode
    (variable-pitch-mode 1)
    (visual-line-mode 1)
    (add-hook 'org-mode-hook #'org-modern-mode)
    (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
    (setq
     ;; Edit settings
     org-auto-align-tags nil
     org-tags-column 0
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

     ;; TODO states

     ;; Org styling, hide markup etc.
     org-hide-emphasis-markers t
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
     "⭠ now ─────────────────────────────────────────────────"

    org-agenda-custom-commands
    '(("d" "Dashboard"
       ((agenda "" ((org-deadline-warning-days 7)))
        (todo "NEXT"
              ((org-agenda-overriding-header "Next Tasks")))
        (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

      ("n" "Next Tasks"
       ((todo "NEXT"
	      ((org-agenda-overriding-header "Next Tasks")))))

      ("W" "Work Tasks" tags-todo "+work-email")

      ;; Low-effort next actions
      ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
       ((org-agenda-overriding-header "Low Effort Tasks")
        (org-agenda-max-todos 20)
        (org-agenda-files org-agenda-files)))

      ("w" "Workflow Status"
       ((todo "WAIT"
	      ((org-agenda-overriding-header "Waiting on External")
	       (org-agenda-files org-agenda-files)))
        (todo "REVIEW"
	      ((org-agenda-overriding-header "In Review")
	       (org-agenda-files org-agenda-files)))
        (todo "PLAN"
	      ((org-agenda-overriding-header "In Planning")
	       (org-agenda-todo-list-sublevels nil)
	       (org-agenda-files org-agenda-files)))
        (todo "BACKLOG"
	      ((org-agenda-overriding-header "Project Backlog")
	       (org-agenda-todo-list-sublevels nil)
	       (org-agenda-files org-agenda-files)))
        (todo "READY"
	      ((org-agenda-overriding-header "Ready for Work")
	       (org-agenda-files org-agenda-files)))
        (todo "ACTIVE"
	      ((org-agenda-overriding-header "Active Projects")
	       (org-agenda-files org-agenda-files)))
        (todo "COMPLETED"
	      ((org-agenda-overriding-header "Completed Projects")
	       (org-agenda-files org-agenda-files)))
        (todo "CANC"
	      ((org-agenda-overriding-header "Cancelled Projects")
	       (org-agenda-files org-agenda-files))))))

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




(use-package org
    :commands (org-capture org-agenda)
    :hook (org-mode . caspeer/org-mode-setup)
    :bind (("C-c l" . org-store-link)
           ("C-c c" . org-capture)
           ("C-M-|" . indent-rigidly))
    :config
    (global-set-key "\C-cb" 'org-switchb)
    (require 'org-habit)
    (add-to-list 'org-modules 'org-habit)
    (setq org-habit-graph-column 60)
    ;; Save Org buffers after refiling!
    (advice-add 'org-refile :after 'org-save-all-org-buffers)

    (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
       "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal"
       entry (function today-journal-file)
        "* %<%Y-%m-%d>: \n %? "
        :empty-lines 1
        :clock-in t
        :clock-resume t)
      
      ))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" )
            ))
    ;; Configure custom agenda views
    (global-set-key "\C-ca" 'org-agenda)
    (define-key global-map (kbd "C-c j")
		(lambda () (interactive) (org-capture nil "jj")))
    (caspeer/org-font-setup)
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


(defvar org-electric-pairs '((?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")
(defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'org-add-electric-pairs)

;;Org-Roam
(use-package org-roam
    :ensure t
    :after org
    :custom
    (org-roam-directory (file-truename "~/Documents/zettle"))
    (org-roam-completion-everywhere t)
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           (("C-c q" . #'org-roam-tag-add))
           ("C-c n v" . #'org-roam-node-visit)
           ;; Dailies
	   ("C-c j" . org-roam-dailies-capture-today)
	   ("C-c d" . org-roam-dailies-goto-today)) ;
    :config
    ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-autosync-mode)
    (setq org-roam-file-extensions '("org" "org_archive")
	  org-id-extra-files (org-roam-list-files)
	  org-roam-dailies-directory "journal/")
    (setq org-roam-capture-templates
	  '(("f" "fleeting" plain "%?"
	     :if-new (file "inbox.org"
				"#+title: ${title}\n"
				"#+date: ${date}\n")
	     :immediate-finish t
             :unnarrowed t
	     )))
    ;; If using org-roam-protocol
    (require 'org-roam-protocol))


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

;; 


