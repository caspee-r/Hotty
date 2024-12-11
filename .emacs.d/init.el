;; Startup Performance
;; The default is 800 kilobytes.  Measured in bytes.

;; Package System Setup
(require 'package)
(setq package-archives '(
                         ("melpa" .  "http://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ))

(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))

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

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(load-user-file "settings.el")
(load-user-file "utils.el")
;;(load-user-file "lsp.el")
;; (load-user-file "evil.el")
;;(load-user-file "org-mode.el")
(add-hook 'org-mode-hook (load-user-file "org-mode.el"))
(defvar custom-tab-width 4 "the width of a tab character")

(defun disable-tabs () (setq indent-tabs-mode nil))

(defun enable-tabs  ()
  ;;(local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key (kbd "TAB") 'indent-for-tab-command)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(setq-default electric-indent-inhibit t)

(add-hook 'prog-mode-hook 'enable-tabs)

;; (global-set-key (kbd "TAB") 'tab-to-tab-stop)

(setq c-mode-indent-offset custom-tab-width)
(add-hook 'c-mode-hook 'subword-mode)
(setq backward-delete-char-untabify-method 'hungry)


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

(use-package no-littering
			 :ensure t)

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

;;(set-face-background 'cursor "#c96")
;; (set-face-background 'default "#111")
;;(set-face-background 'isearch "#ff0")
;;(set-face-foreground 'isearch "#000")
;; (set-face-background 'lazy-highlight "#990")
;; (set-face-foreground 'lazy-highlight "#000")
;;(set-face-foreground 'font-lock-comment-face "#fc0")

;; show trailing whitespace
;; (setq-default show-trailing-whitespace t)

(show-paren-mode)
(column-number-mode)
;; Set frame transparency
;;(defvar caspeer/frame-transparency '(90 . 90))
;;(set-frame-parameter (selected-frame) 'alpha caspeer/frame-transparency)
;;(add-to-list 'default-frame-alist `(alpha . ,caspeer/frame-transparency))
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; line-mode
;; numbers for programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

;; General Keybindings
(defun mp-elisp-mode-eval-buffer ()
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)

;; kill this buffer
(global-set-key [(control x) (k)] 'kill-this-buffer)
;-----------

;; a nice package for coloring
(use-package rainbow-delimiters
    :ensure t
    :hook (prog-mode . rainbow-delimiters-mode))

;; Term Mode -----------------------------------------
(use-package eterm-256color
			 :hook (term-mode . eterm-256color-mode))

;; Vterm

(use-package vterm
			 :commands vterm
			 :config
			 (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
			 ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
			 (setq vterm-max-scrollback 10000))

;;
;;
;;(use-package vertico
;;			 :ensure t
;;			 :config
;;			 (vertico-mode 1)
;;			 (vertico-prescient-mode 1)
;;
;;			 )
;;
;;(use-package vertico-prescient
;;			 :ensure t
;;			 )
;;
(use-package prescient
	:ensure t
	)

;; Which Key??
(use-package which-key
			 :defer 0
			 :diminish which-key-mode
			 :config
			 (setq which-key-idle-delay 1))
;; is useful for displaying a panel showing each key binding you use in a panel on the right side of the frame.  Great for live streams and screencasts!


(use-package multiple-cursors
	:ensure t
	)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)


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

(electric-pair-mode 1)

;; Global HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-face-attribute 'fill-column-indicator   nil :foreground "#6fa855" :background nil)
(setq
 fill-column 90
 global-display-fill-column-indicator-character ?█
 global-display-fill-column-indicator-modes '(prog-mode)
											  )
;(global-display-fill-column-indicator-mode t)

;; Dired ------------------------
(require 'dired-x)
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


(use-package dired-single
			 :commands (dired dired-jump))

;; (use-package all-the-icons-dired
;; 			 :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
			 :commands (dired dired-jump)
			 :config
			 ;; Doesn't work as expected!
			 ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
			 (setq dired-open-extensions '(("png" . "feh")
										   ("mkv" . "mpv"))))

;;;  Registers
(setq register-preview-delay 0)


(use-package helm
	 :init
	 (custom-set-variables '(helm-command-prefix-key "C-;"))
	:config
	(setq
	 helm-input-idle-delay					   0.01
	 helm-idle-delay                           0.01
	 helm-quick-update                         t
	 helm-reuse-last-window-split-state		   t
	 helm-split-window-default-side            'other
	 helm-always-two-windows				   t
	 helm-split-window-inside-p				   t
	 helm-mode-fuzzy-match					   t
	 helm-completion-in-region-fuzzy-match     t
	 helm-follow-mode-persistent               t
	 completions-detailed                      t
	 history-delete-duplicates                 t
	 helm-ff-search-library-in-sexp            t
	 helm-ff-file-name-history-use-recentf     t
	 helm-buffers-fuzzy-matching               t
	 ido-use-virtual-buffers                   t
	 )
	(helm-mode 1)
	:bind
	(
	 (:map helm-map
		   ("C-o" . nil)
           ("TAB" . helm-execute-persistent-action)
           ("C-i" . helm-execute-persistent-action)
           ("C-z" . helm-select-action)
 		   )
	 (:map global-map ("M-x" . 'helm-M-x)
	 ("M-y" . 'helm-show-kill-ring)
	 ("C-x C-f" . 'helm-find-files)
	 ("C-x b" . 'helm-buffers-list))
	 )
	)



;; (use-package ido
;;     :init
;;     (setq ido-enable-flex-matching t
;; 		  ido-auto-merge-work-directories-length -1
;; 		  ido-create-new-buffer 'always
;; 		  ido-use-filename-at-point 'guess
;; 		  ido-everywhere t
;; 		  ido-default-buffer-method 'selected-window)
;;     :config
;;     (ido-mode 1)
;;     (ido-everywhere 1)
;;     (put 'ido-exit-minibuffer 'disabled nil)
;;     (when (require 'ido-ubiquitous nil t)
;; 		(ido-ubiquitous-mode 1))
;;     (fido-mode 1)
;;
;;     )


;; (use-package flx-ido
;;     :ensure t
;;     :init (setq ido-enable-flex-matching t
;;                 ido-use-faces nil)
;;     :config (flx-ido-mode 1)
;;     )


;; (use-package icomplete-vertical
;; 	:config
;; 	(fido-mode 1)
;; 	(icomplete-vertical-mode 1)
;; 	)

;; Snippets
(use-package yasnippet
	:init
	(add-hook 'c-ts-mode-hook
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

(use-package corfu
	:ensure t
	:bind 	(:map corfu-map
	 	 		  ("C-n" . 'corfu-next)
				  ("C-p" . 'corfu-previous)
				  ("<escape>" . 'corfu-quit)
				  ("<return>" . 'corfu-insert)
				  ("M-d" .  'corfu-show-documentation)
				  ("M-l" . 'corfu-show-location)
				  )
	:init
	(global-corfu-mode)
	:config
	;; Enable auto completion and configure quitting
	(setq corfu-auto t
		  global-corfu-modes '((not erc-mode
									circe-mode
									help-mode
									gud-mode
									vterm-mode)
                               t)
		  corfu-cycle t
		  corfu-max-width corfu-min-width
          corfu-preselect 'prompt
          corfu-count 16
          corfu-max-width 120
          corfu-on-exact-match nil
          tab-always-indent t
		  corfu-show-documentation t
		  corfu-quit-at-boundary 'seperator
		  )
	(add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles))))

(use-package yasnippet-capf
	:defer t
	:config
	(add-hook 'yas-minor-mode-hook
			   (defun +corfu-add-yasnippet-capf-h ()
		(add-hook 'completion-at-point-functions #'yasnippet-capf 30 t)))
	)

(use-package cape
	:init
	(add-hook 'prog-mode-hook
			  (defun +corfu-add-cape-file-h ()
				  (add-hook 'completion-at-point-functions #'cape-file -10 t)))
	;; Add to the
	;; global default value of `completion-at-point-functions' which is
	;; used by `completion-at-point'.  The order of the functions matters, the
	;; first function returning a result wins.  Note that the list of buffer-local
	;; completion functions takes precedence over the global list.
	(add-to-list 'completion-at-point-functions #'cape-dabbrev)
	(add-to-list 'completion-at-point-functions #'cape-file)
	;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
	(add-to-list 'completion-at-point-functions #'cape-keyword)
	(add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
	;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
	)


;; Languages

(setq
 c-basic-offset 4
 )

(use-package zig-mode
	:config
	(setq zig-format-on-save 'nil)
	)

(use-package doc-view
			 :config
			 (setq
			   doc-view-continuous t
			   doc-view-imenu-enabled t
			   doc-view-mupdf-use-svg t

			   )
			 )

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


(global-set-key [f9] 'compile)

(add-hook 'c-mode-hook
		  (lambda ()
			(unless (file-exists-p "Makefile")
			  (set (make-local-variable 'compile-command)
				   ;; emulate make's .c.o implicit pattern rule, but with
				   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
				   ;; variables:
				   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
				   (let ((file (file-name-nondirectory buffer-file-name)))
					 (format "%s -o %s %s"
							 (or (getenv "CC") "gcc")
							 (file-name-sans-extension file)
							 file))))))


(defun caspeer/compilation-finish-function (buffer status)
  "Bring the compilation window into focus if there are errors."
  (if (and (string-prefix-p "*compilation" (buffer-name buffer))
		   (string-match-p "exited abnormally" status))
	(select-window (get-buffer-window buffer t))))

;; Add the function to the compilation-finish-functions variable
(add-hook 'compilation-finish-functions 'caspeer/compilation-finish-function)

;;compilation

(setq compilation-error-screen-columns nil
	  compilation-auto-jump-to-first-error t
	  )




(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
	(forward-line n)
	(let ((start (point)))
	  (insert line-text)
	  (setq deactivate-mark nil)
	  (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -2 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 2 n)))

(global-set-key (kbd "M-<up>") 'move-region-up)
(global-set-key (kbd "M-<down>") 'move-region-down)



; keymaps
(global-set-key (kbd "<f7>t") 'load-theme)
(global-set-key (kbd "C-c e d") 'eval-defun)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e e") 'eval-expression)

(global-set-key (kbd "C-c SPC") 'async-shell-command)

(global-set-key (kbd "C-c m") 'multi-occur-in-matching-buffers)
;; (global-set-key (kbd "" ) 'next-buffer)
;; (global-set-key (kbd "" ) 'previous-buffer)

(global-hl-line-mode t)
(repeat-mode t)

(put 'set-goal-column 'disabled nil)

;; Buffer
(global-set-key "\M-n"  'next-buffer)
(global-set-key "\M-p"  'previous-buffer)
;; Window
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)

(defun caspeer/rm-this-file ()
  (interactive)
  (delete-file (buffer-file-name))
  (kill-this-buffer)
  )

(global-set-key (kbd "C-c d") 'caspeer/rm-this-file)
(global-set-key (kbd "C-c <insert>") 'insert-char)

(defun caspeer/half-scroll-up ()
	"modifies the scroll-up-command to scroll just for half page"
	(interactive)
	(scroll-up-command (-  (/ (window-size) 2) 5))
	)

(defun caspeer/half-scroll-down ()
	"modifies the scroll-down-command to scroll just for half page"
	(interactive)
	 (scroll-down-command (-  (/ (window-size) 2) 5)))

;;(global-set-key (kbd "C-v") 'caspeer/half-scroll-up)
;;(global-set-key (kbd "M-v") 'caspeer/half-scroll-down)

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
(defun caspeer/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'caspeer/duplicate-line)
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

;; Project
(defun project-find-c-src (dir)
	(when-let ((root (locate-dominating-file dir "Makefile")))
		(cons 'c-src root)))

(cl-defmethod project-root ((project (head c-src)))
	(cdr project)
	)

(add-hook 'project-find-functions #'project-find-c-src)

;;(use-package treesit-auto
;;			 :config
;;			 (treesit-auto-add-to-auto-mode-alist 'all)
;;			 (setq
;;			   treesit-auto-install-all t)
;;			 )

;;(setq treesit-language-source-alist
;;	  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;		(cmake "https://github.com/uyha/tree-sitter-cmake")
;;		(css "https://github.com/tree-sitter/tree-sitter-css")
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


(put 'upcase-region 'disabled nil)
(mouse-avoidance-mode 'jump)

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
		("\\*compilation\\*"
		 (display-buffer-in-direction)
		 (direction . down)
		 (window-height . 0.30)
		 )
		)
	  )
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)

(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
			   "Discard all themes before loading new."
			   (mapc #'disable-theme custom-enabled-themes))

(load-theme 'gruber-darker )
