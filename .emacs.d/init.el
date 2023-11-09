;; Startup Performance
;; The default is 800 kilobytes.  Measured in bytes.

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

(load-user-file "settings.el")
(load-user-file "utils.el")
(load-user-file "lsp.el")
;; (load-user-file "evil.el")
(load-user-file "org-mode.el")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; (setq disabled-command-function nil)


(use-package no-littering)

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


;; UI CONFIGURATION
(setq inhibit-startup-message t)
(tool-bar-mode -1) ;Disable the toolbar
(menu-bar-mode -1);Disable the menu bar
(scroll-bar-mode -1);Disable the scrolbar
(tooltip-mode -1);Disable the toolbar
(load-theme 'gruber-darker)

;; (set-fringe-mode 10)  give some breathing room

(set-face-background 'cursor "#c96")
(set-face-background 'default "#111")
(set-face-background 'isearch "#ff0")
(set-face-foreground 'isearch "#000")
;; (set-face-background 'lazy-highlight "#990")
;; (set-face-foreground 'lazy-highlight "#000")
(set-face-foreground 'font-lock-comment-face "#fc0")

;; show trailing whitespace
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default require-final-newline t)
;; Single Space for Sentence Spacing
(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode t)

(setq c-basic-offset 4)

(setq show-paren-delay 0)
(show-paren-mode)

;; Set up the visible bell
(setq visible-bell t)
(column-number-mode)
;; Set frame transparency
(defvar efs/frame-transparency '(90 . 90))
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))



(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)
;; Make frame transparency overridable
;; relative numbers
(setq display-line-numbers-type 'relative)
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
(use-package all-the-icons)

;; statusline
(use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 15)))

;; a nice package for coloring ([{
(use-package rainbow-delimiters
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


(use-package command-log-mode
    :commands command-log-mode)





;; MAGIT
(use-package magit
    :commands magit-status
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package recentf
    :init
    (setq recentf-max-menu-items 25
          recentf-auto-cleanup 'never
          recentf-keep '(file-remote-p file-readable-p))
    (recentf-mode 1)
    (let ((last-ido "~/.emacs.d/ido.last"))
        (when (file-exists-p last-ido)
            (delete-file last-ido)))
    :bind ("C-c f r" . recentf)
    )

(electric-pair-mode 1)

;; Dired ------------------------
(use-package dired
    :ensure nil
    :hook ('dired-mode-hook 'auto-revert-mode)
    :commands (dired dired-jump)
    :bind (("C-x C-j" . dired-jump))
    :custom ((dired-listing-switches "-agho --group-directories-first"))
)

(use-package dired-single
    :commands (dired dired-jump))

(use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
    :commands (dired dired-jump)
    :config
    ;; Doesn't work as expected!
    ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
    (setq dired-open-extensions '(("png" . "feh")
                                  ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode)
)

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

(use-package flx-ido
    :ensure t
    :init (setq ido-enable-flex-matching t
                ido-use-faces nil)
    :config (flx-ido-mode 1)
    )


;; Snippets
(use-package yasnippet
    :config
    (yas-global-mode 1)
    )
(use-package yasnippet-snippets)


(use-package company
	:defer 0.1
    ;; :hook ((lsp-mode emacs-lisp-mode) . company-mode)
    :bind (:map company-active-map
				("<Tab>" . company-complete-selection))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0)
    :config
	(global-company-mode)
    (setq company-selection-wrap-around t)
    )



(use-package company-quickhelp
    :after company
    :hook (company-mode . company-quickhelp-mode )

     )


;; Languages
(use-package python-mode
    :ensure nil
    :hook (python-mode . lsp)
    :custom
    (python-shell-interpreter "python3"))

;;Lispy
(use-package lispy
	:init
	(add-hook 'lisp-interaction-mode-hook 'lispy-mode)
	(add-hook 'eval-expression-minibuffer-setup-hook 'lispy-mode)
    :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode ) . lispy-mode)
    :config
    (lispy-mode t)

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
          (shrink-window (- h 9))

		  )

      ))
(global-set-key [f9] 'my-recompile)

(add-hook 'c-mode-hook
           (lambda ()
	     (unless (file-exists-p "Makefile")
	       (set (make-local-variable 'compile-command)
                    ;; emulate make's .c.o implicit pattern rule, but with
                    ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                    ;; variables:
                    ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		    (let ((file (file-name-nondirectory buffer-file-name)))
                      (format "%s -c -o %s.o %s %s %s"
                              (or (getenv "CC") "gcc")
                              (file-name-sans-extension file)
                              (or (getenv "CPPFLAGS") "-DDEBUG=9")
                              (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
			      file))))))

(setq compilation-scroll-output 'first-error)
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
										; keymaps
(global-set-key (kbd "<f7>t") 'load-theme)
(global-set-key (kbd "C-c e d") 'eval-defun)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e e") 'eval-expression)

(global-set-key (kbd "C-.") 'compilation )

(global-set-key (kbd "C-.") 'scratch-buffer)
;; (global-set-key (kbd "" ) 'next-buffer)
;; (global-set-key (kbd "" ) 'previous-buffer)
(global-hl-line-mode t)
(repeat-mode t)

(put 'set-goal-column 'disabled nil)
