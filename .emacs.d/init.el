;; Startup Performance
;; The default is 800 kilobytes.  Measured in bytes.

;; Package System Setup
(require 'package)
(setq
 package-archives '(("melpa" . "http://melpa.org/packages/")
					("gnu"   . "https://elpa.gnu.org/packages/")
					("org"   . "https://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defconst user-init-dir
		  (cond ((boundp 'user-emacs-directory)
				 user-emacs-directory)
				((boundp 'user-init-directory)
				 user-init-directory)
				(t "~/.emacs.d/")))

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

(defun load-user-file (file)
	(interactive "f")
	"Load a file in current user's configuration directory"
	(load-file (expand-file-name file "~/.emacs.d")))

;; Font Familly
(set-face-attribute 'default nil :font "Iosevka Term" :height 150)



;; UI CONFIGURATION
(setq inhibit-startup-message t
	  use-dialog-box nil
	  warning-minimum-level :emergency
	  display-line-numbers-type 'relative
	  )
(tool-bar-mode -1) ;Disable the toolbar
(menu-bar-mode -1);Disable the menu bar
(scroll-bar-mode -1);Disable the scrolbar
(tooltip-mode -1);Disable the tooltip

(fset 'yes-or-no-p 'y-or-n-p) ;; y or n

(defvar custom-tab-width 4 "the width of a tab character")

;; Good Settings
(setq
 scroll-margin 1
 scroll-step 1
 scroll-preserve-screen-position 'always
 scroll-conservatively 1000
 next-line-add-newlines t
 load-prefer-newer t
 require-final-newline t
 save-interprogram-paste-before-kill t
 switch-to-buffer-obey-display-actions nil
 list-matching-lines-default-context-lines 3
 sentence-end-double-space t
 compilation-scroll-output t ;; ?
 set-mark-command-repeat-pop t
 backward-delete-char-untabify-method 'hungry
 c-mode-indent-offset custom-tab-width
 )

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Minor Modes
(savehist-mode 1)
(save-place-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(column-number-mode)
(electric-pair-mode 1)
(global-hl-line-mode t)
(repeat-mode t)
(mouse-avoidance-mode 'jump)

;; Backups
(setq
 backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/" user-emacs-directory)))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t )

;; Auto Save
(make-directory (expand-file-name ".tmp/auto-saves/" user-emacs-directory) t)
(setq
 auto-save-list-file-prefix (expand-file-name ".tmp/auto-saves/sessions/" user-emacs-directory)
 auto-save-file-name-transforms `((".*" ,(expand-file-name ".tmp/auto-saves/" user-emacs-directory) t)))

;; Bookmarks
(setq
 bookmark-save-flag t
 bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))

(load-user-file "org-mode.el")

(defun disable-tabs () (setq indent-tabs-mode nil))

(defun enable-tabs  ()
  ;;(local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key (kbd "TAB") 'indent-for-tab-command)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))


;;(setq-default electric-indent-inhibit t)

(add-hook 'prog-mode-hook 'enable-tabs)

;; (global-set-key (kbd "TAB") 'tab-to-tab-stop)

(use-package simpc-mode
	:load-path "/home/caspeer/.emacs.d/"
	;:hook ('simpc-mode-hook . )

	)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
;;(add-hook 'simpc-mode-hook 'subword-mode)


(use-package whitespace
			 :bind ("C-c t w" . whitespace-mode)
			 :init
			 (setq whitespace-line-column nil
				   whitespace-display-mappings '((space-mark 32 [183] [46])
												 (newline-mark 10 [9166 10])
												 (tab-mark 9 [9654 9] [92 9])))
			 :config
			 (set-face-attribute 'whitespace-space       nil :foreground "#666666" :background nil)
			 (set-face-attribute 'whitespace-newline     nil :foreground "#666666" :background nil)
			 (set-face-attribute 'whitespace-indentation nil :foreground "#666666" :background nil)
			 (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))
			 :diminish whitespace-mode)

(use-package expand-region
	:bind
	("C-=" . 'er/expand-region)
	)

(use-package winner
			 :init
			 (setq winner-dont-bind-my-keys t)
			 (winner-mode 1)
			 :bind (("C-c u" . winner-undo)
					("C-c r" . winner-redo))
			 )


;; Hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))



;-----------

(use-package vterm
			 :commands vterm
			 :config
			 (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
			 ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
			 (setq vterm-max-scrollback 10000))


(use-package prescient
	:ensure t
	)

;; Which Key??
(use-package which-key
			 :defer 0
			 :diminish which-key-mode
			 :config
			 (setq which-key-idle-delay 1))

(use-package multiple-cursors
	:ensure t
	:bind (
		   ("C-S-c C-S-c" . 'mc/edit-lines)
		   ("C->"         . 'mc/mark-next-like-this)
		   ("C-<"         . 'mc/mark-previous-like-this)
		   ("C-c C-<"     . 'mc/mark-all-like-this)
		   ("C-\""        . 'mc/skip-to-next-like-this)
		   ("C-:"         . 'mc/skip-to-previous-like-this)
		   )
	)

;; MAGIT
(use-package magit
			 :ensure t
			 :commands magit-status
			 :custom
			 (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun filter-symbolic-links (paths)
  "Return a new list from PATHS with duplicates removed where files and symbolic links point to the same file."
  (let ((unique-paths '()))
    (dolist (path paths)
      (let ((true-path (file-truename path)))
        ;; Only add the path if its true path is not already in the unique-paths list
        (unless (member true-path unique-paths)
          (push true-path unique-paths))))
    ;; Return the unique paths as a new list in the original order
    (nreverse unique-paths)))

(defun recentf-find-file (&optional filter)
  "Find a recent file using `completing-read'.
When optional argument FILTER is a function, it is used to
transform recent files before completion."
  (interactive)
  (let* ((filter (if (functionp filter) filter #'abbreviate-file-name))
         (file (completing-read "Choose recent file: "
                                (delete-dups (mapcar filter recentf-list))
                                nil t)))
    (when file
      (find-file file))))

(use-package recentf
	:ensure nil
	:config
	(setq recentf-max-menu-items 20
		  recentf-max-saved-items 100
		  recentf-keep '(file-remote-p file-readable-p file-exists-p)
		  )
	(recentf-mode t)
	:bind ("C-c f r" . 'recentf-find-file)
	)


;; Global HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Dired ------------------------
(use-package dired
			 :ensure nil
			 :hook ('dired-mode-hook 'auto-revert-mode)
			 :commands (dired dired-jump)
			 :bind (:map  dired-mode-map
						  ("-" . 'dired-up-directory)
						  ("C-x C-j" . dired-jump))

			 :custom ((dired-listing-switches "-aghlt ")) ;;--group-directories-first
			 :config
			 (setq dired-dwim-target t)
			 )

;;;  Registers
(setq register-preview-delay 0)


(use-package ido
	:init
	(setq
	 ido-enable-flex-matching t
	 ido-auto-merge-work-directories-length -1
	 ido-create-new-buffer 'always
	 ido-use-filename-at-point 'guess
	 ido-everywhere t
	 ido-default-buffer-method 'selected-window)
	:config
	(ido-mode 1)
	(ido-everywhere 1)
	(put 'ido-exit-minibuffer 'disabled nil)
	(fido-mode 1)
	)

;; HIPPIE
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(setq hippie-expand-verbose nil)

;; Snippets
(use-package yasnippet
	:init
	(add-hook 'simpc-mode-hook
			  (lambda ()
				  (setq-local yas--major-mode 'c-mode)
				  (yas-activate-extra-mode 'c-mode)))
	:bind
	(:map yas-minor-mode-map
		  ("C-'". yas-expand)
		  ([(tab)] . nil)
		  ("TAB" . nil))
	:config
	(yas-global-mode 1)
	)

(use-package yasnippet-snippets
	:ensure t)

(use-package undo-tree
	:ensure t
	:hook
	(prog-mode . undo-tree-mode)
	:config
	(setq
	 undo-tree-visualizer-diff t
	 undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo"))
	 )
	)

(global-set-key (kbd "C-x c") 'compile)

(add-hook 'simpc-mode-hook
		  (lambda ()
			  (set (make-local-variable 'compile-command)
				   (cond
					;; If "Makefile" exists, use "make -B"
					((file-exists-p "Makefile")
					 "make -B")
					;; Else if "build.sh" exists, use "./build.sh"
					((file-exists-p "build.sh")
					 "./build.sh")
					;; Else use default GCC compile command
					(t
					 (format "gcc %s -o %s"
							 (file-name-nondirectory buffer-file-name)
							 (file-name-sans-extension (file-name-nondirectory buffer-file-name)))))))
		  )

;;Compilation
(setq compilation-error-screen-columns nil
	  compilation-auto-jump-to-first-error t
	  )

; Global Keymaps
(global-set-key (kbd "C-c e d"	) 'eval-defun)
(global-set-key (kbd "C-c e r"	) 'eval-region)
(global-set-key (kbd "C-c e e"	) 'eval-expression)
(global-set-key (kbd "C-c e b"	) 'eval-buffer)
(global-set-key (kbd "C-c SPC"	) 'async-shell-command)
(global-set-key (kbd "C-c m"	) 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-x k"	) 'kill-current-buffer)
(global-set-key "\M-n"  'next-buffer)
(global-set-key "\M-p"  'previous-buffer)
;(global-set-key (kbd "C-.") #'other-window)
;(global-set-key (kbd "C-,") #'prev-window)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)


(defun caspeer/rm-this-file ()
  (interactive)
  (delete-file (buffer-file-name))
  (kill-this-buffer)
  )

(global-set-key (kbd "C-c d") 'caspeer/rm-this-file)
(global-set-key (kbd "C-c <insert>") 'insert-char)

(defun toggle-window-split ()
	(interactive)
	(if (= (count-windows) 2)
			(let* ((this-win-buffer (window-buffer))
				   (next-win-buffer (window-buffer (next-window)))
				   (this-win-edges (window-edges (selected-window)))
				   (next-win-edges (window-edges (next-window)))
				   (this-win-2nd (not (and (<= (car this-win-edges)
											   (car next-win-edges))
										   (<= (cadr this-win-edges)
											   (cadr next-win-edges)))))
				   (splitter
					(if (= (car this-win-edges)
						   (car (window-edges (next-window))))
							'split-window-horizontally
						'split-window-vertically)))
				(delete-other-windows)
				(let ((first-win (selected-window)))
					(funcall splitter)
					(if this-win-2nd (other-window 1))
					(set-window-buffer (selected-window) this-win-buffer)
					(set-window-buffer (next-window) next-win-buffer)
					(select-window first-win)
					(if this-win-2nd (other-window 1))))))


(global-set-key (kbd "C-x |") 'toggle-window-split)
(global-set-key (kbd "C-c w i") 'imenu)

(global-set-key (kbd "C-c f o") 'ff-find-other-file)

(defadvice kill-line (before kill-line-autoreindent activate)
	"Kill excess whitespace when joining lines.
		   If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
	(when (and (eolp) (not (bolp)))
		(save-excursion
			(forward-char 1)
			(just-one-space 1))))

(defadvice kill-ring-save (before slick-copy activate compile)
		   "When called interactively with no active region, copy a single line instead."
		   (interactive
			 (if mark-active (list (region-beginning) (region-end))
			   (message "Single line killed")
			   (list (line-beginning-position)
					 (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
		   "When called interactively with no active region, kill a single line instead."
		   (interactive
			 (if mark-active (list (region-beginning) (region-end))
			   (list (line-beginning-position)
					 (line-beginning-position 2)))))

(defadvice backward-kill-word (around delete-pair activate)
		   (if (eq (char-syntax (char-before)) ?\()
			 (progn
			   (backward-char 1)
			   (save-excursion
				 (forward-sexp 1)
				 (delete-char -1))
			   (forward-char 1)
			   (append-next-kill)
			   (kill-backward-chars 1))
			 ad-do-it))

(use-package rfc-mode)

(use-package mu4e
	:ensure nil
	:config
	(setq mu4e-change-filenames-when-moving t
		  mu4e-update-interval (* 10 60)
		  mu4e-get-mail-command "mbsync -a"
		  mu4e-mail-dir "~/.mail/gmail"
		  mu4e-drafts-folder "/[Gmail].Drafts"
		  mu4e-sent-folder "/[Gmail].Sent Mail"
		  mu4e-refile-folder "/[Gmail].All Mail"
		  mu4e-trash-folder "/[Gmail].Trash"
		  )
	(setq mu4e-maildir-shortcuts
		  '(
			("/Inbox" . ?i)
			("/[Gmail].Drafts" . ?d)
			("/[Gmail].Sent Mail" . ?s)
			("/[Gmail].All Mail" . ?a)
			("/[Gmail].Trash" . ?t)
			)

		  )

	)

;;(use-package treesit-auto
;;			 :config
;;			 (treesit-auto-add-to-auto-mode-alist 'all)
;;			 (setq
;;			   treesit-auto-install-all t)
;;			 )

;;(setq treesit-language-source-alist
;;	  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;		(cmake "https://github.com/uyha/tree-sitter-cmake")
;;		(css "https://xgithub.com/tree-sitter/tree-sitter-css")
;;		(elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;		(go "https://github.com/tree-sitter/tree-sitter-go")
;;		(html "https://github.com/tree-sitter/tree-sitter-html")
;;		(c "https://github.com/tree-sitter/tree-sitter-c")
;;		(python "https://github.com/tree-sitter/tree-sitter-python")
;;		(lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
;;		(gas "https://github.com/sirius94/tree-sitter-gas")
;;		(ada "https://github.com/briot/tree-sitter-ada")
;;		)
;;	  )
;;(setq	treesit-font-lock-level 4)
;; (setq major-mode-remap-alist
;; 	  '(
;; 		(asm-mode . gas-ts-mode)
;; 		)
;; 	  )

(defun fixed-native-compile-async-skip-p
        (native-compile-async-skip-p file load selector)
    (let* ((naive-elc-file (file-name-with-extension file "elc"))
           (elc-file       (replace-regexp-in-string
                               "\\.el\\.elc$" ".elc" naive-elc-file)))
        (or (gethash elc-file comp--no-native-compile)
            (funcall native-compile-async-skip-p file load selector))))

(advice-add 'native-compile-async-skip-p
    :around 'fixed-native-compile-async-skip-p)

(setq display-buffer-alist
	  '(

		("\\*undo-tree\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.40)
		 )
		("\\*grep\\*"
		 (display-buffer-in-direction)
		 (direction . down)
		 (window-height . 0.50)
		 )
		)
	  )
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)

;; remap C-c C-b to Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-unset-key (kbd "C-x C-c"))
(setq ibuffer-expert t)

(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
			   "Discard all themes before loading new."
			   (mapc #'disable-theme custom-enabled-themes))

(load-theme 'gruber-darker)

(set-face-background 'cursor "#1aed84")
;; (set-face-background 'default "#111")
;;(set-face-background 'isearch "#ff0")
;;(set-face-foreground 'isearch "#000")
;; (set-face-background 'lazy-highlight "#990")
;; (set-face-foreground 'lazy-highlight "#000")
;;(set-face-foreground 'font-lock-comment-face "#fc0")
