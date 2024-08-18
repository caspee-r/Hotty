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

(setq c-ts-mode-indent-offset custom-tab-width)
(add-hook 'c-ts-mode-hook 'subword-mode)
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

;; icons package
;; (use-package all-the-icons)


;; a nice package for coloring ([{
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



;; Ivy and Counsel Configuration -----------------------------------------------------------

;; (use-package ivy
;;     :bind (("C-s" . swiper)
;;            :map ivy-minibuffer-map
;;            ("TAB" . ivy-alt-done)
;;            ("C-l" . ivy-alt-done)
;;            ("C-j" . ivy-next-line)
;;            ("C-k" . ivy-previous-line)
;;            :map ivy-switch-buffer-map
;;            ("C-k" . ivy-previous-line)
;;            ("C-l" . ivy-done)
;;            ("C-d" . ivy-switch-buffer-kill)
;;            :map ivy-reverse-i-search-map
;;            ("C-k" . ivy-previous-line)
;;            ("C-d" . ivy-reverse-i-search-kill))
;;     :config
;;     (ivy-mode 1)
;;     (setq ivy-use-virtual-buffers t
;; 	  ivy-count-format "%d/%d "))

;; (use-package ivy-rich
;;     :after ivy
;;     :init
;;     (ivy-rich-mode 1))

;; (use-package counsel
;;     :bind (("M-x" . counsel-M-x)
;;            ("C-x b" . counsel-ibuffer)
;;            ("C-x C-f" . counsel-find-file)
;;            :map minibuffer-local-map
;;            ("C-r" . 'counsel-minibuffer-history))
;;     :custom
;;     (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
;;     :config
;;     (counsel-mode 1))

;; (use-package ivy-prescient
;;     :after counsel
;;     :custom
;;     (ivy-prescient-enable-filtering nil)
;;     :config
;;     ;; Uncomment the following line to have sorting remembered across sessions!
;; 					;(prescient-persist-mode 1)
;;     (ivy-prescient-mode 1)
;;     )

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
;;(use-package prescient
;;			 :ensure t
;;			 )
;;


(use-package helpful
			 :commands (helpful-callable helpful-variable helpful-command helpful-key)
			 :bind
			 ([remap describe-function] . helpful-function)
			 ([remap describe-command] . helpful-command)
			 ([remap describe-variable] . helpful-variable)
			 ([remap describe-key] . helpful-key))


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

(defun recentf-ido-find-file ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file
   (ido-completing-read "Recentf open: "
                        (mapcar 'abbreviate-file-name recentf-list)
                        nil t)))



(use-package recentf
			 :init
			 (setq recentf-max-menu-items 25
				   recentf-auto-cleanup 'never
				   ido-use-virtual-buffers t
				   recentf-keep '(file-remote-p file-readable-p))
			 (recentf-mode 1)
			 (let ((last-ido "~/.cache/emacs/ido.last"))
			   (when (file-exists-p last-ido)
				 (delete-file last-ido)))
			 :bind ("C-c f r" . recentf-ido-find-file
					)
			 )

(electric-pair-mode 1)

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


;; (use-package dired-hide-dotfiles
;;     :hook (dired-mode . dired-hide-dotfiles-mode)
;; 	:bind
;; 	)

 (use-package ido
     :init
     (setq ido-enable-flex-matching t
 		  ido-auto-merge-work-directories-length -1
 		  ido-create-new-buffer 'always
 		  ido-use-filename-at-point 'guess
 		  ido-everywhere t
 		  ido-default-buffer-method 'selected-window)
     :config
     (ido-mode 1)
     (ido-everywhere 1)
     (put 'ido-exit-minibuffer 'disabled nil)
     (when (require 'ido-ubiquitous nil t)
 		(ido-ubiquitous-mode 1))
     (fido-mode 1)
     )

(use-package smex
	:ensure t)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)



 (use-package flx-ido
     :ensure t
     :init (setq ido-enable-flex-matching t
                 ido-use-faces nil)
     :config (flx-ido-mode 1)
     )


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

	:config
	(yas-global-mode 1)
	)
(use-package yasnippet-snippets
			 :ensure t)


;; (use-package company
;; 	:defer 0.1
;;     :hook ((prog-mode lsp-mode) . company-mode)
;;     :bind (:map company-active-map
;; 				("<Tab>" . company-complete-selection)
;; 				("C-c h" . #'company-quickhelp-manual-begin)
;; 				)
;;     :custom
;;     (company-minimum-prefix-length 1)
;;     (company-idle-delay 0.0)
;;     :config
;; 	(company-prescient-mode)
;;     (setq company-selection-wrap-around t)
;; 	(setq company-format-margin-function #'company-vscode-dark-icons-margin)
;; 	(setq company-backends
;; 		  '((
;; 			 :separate
;; 			 company-yasnippet
;; 			 company-dabbrev
;; 	 		 company-capf
;; 			 company-files
;; 			 company-keywords
;; 			 company-semantic
;; 			 ))
;; 		  )
;; 	)
;; (use-package company-quickhelp
;;     :after company
;;     :hook (company-mode . company-quickhelp-mode )

;;      )


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
          tab-always-indent 'complete
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
	

;; (use-package company-prescient
;; 	:ensure t
;; 	)




;; Languages

(use-package c-mode
	:config
	(setq c-default-style "linux"
		  c-basic-offset 4
		  )
	)


(use-package python-mode
			 :ensure t
			 :hook (python-mode . lsp)
			 :custom
			 (python-shell-interpreter "python3"))

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


;; compilation mode
(require 'compile)

(defun my-recompile ()
  "Run compile and resize the compile window closing the old one if necessary"
  (interactive)
  (progn
	(if (get-buffer "*compilation*") ; If old compile window exists
	  (progn
		(delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
		(kill-buffer "*compilation*") ; and kill the buffers
		)
	  )
	(progn
	  (call-interactively 'compile)
	  (setq cur (selected-window))
	  (setq w (get-buffer-window "*compilation*"))
	  (select-window w)
	  (setq h (window-height w))
	  (shrink-window (- h 14))

	  )
	))
(global-set-key [f9] 'my-recompile)

(add-hook 'c-ts-mode-hook
		  (lambda ()
			(unless (file-exists-p "Makefile")
			  (set (make-local-variable 'compile-command)
				   ;; emulate make's .c.o implicit pattern rule, but with
				   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
				   ;; variables:
				   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
				   (let ((file (file-name-nondirectory buffer-file-name)))
					 (format "%s -o %s.o %s"
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

(defun my-compilation-mode-hook ()
  ;; Jump at the right column even for tab indentation
  (setq compilation-error-screen-columns nil)
  )
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)


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

(global-set-key (kbd "C-v") 'caspeer/half-scroll-up)
(global-set-key (kbd "M-v") 'caspeer/half-scroll-down)

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




;; (defadvice text-scale-increase (around all-buffers (arg) activate)
;; 	(dolist (buffer (buffer-list))
;; 		(with-current-buffer buffer
;; 			ad-do-it)))

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

(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
			   "Discard all themes before loading new."
			   (mapc #'disable-theme custom-enabled-themes))

(load-theme 'kanagawa )
(put 'upcase-region 'disabled nil)
(mouse-avoidance-mode 'jump)

(setq display-buffer-alist
	  '(
		("\\*undo-tree\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.40)
		 )
		("\\*compilation\\*"
		 (display-buffer-in-direction)
		 (direction . down)
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
