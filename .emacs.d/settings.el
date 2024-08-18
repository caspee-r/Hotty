;; helps to keep .emacs.d clean
(make-directory (expand-file-name ".tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name ".tmp/auto-saves/sessions/" user-emacs-directory)
	  auto-save-file-name-transforms `((".*" ,(expand-file-name ".tmp/auto-saves/" user-emacs-directory) t)))
(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/" user-emacs-directory))))

;; Font Familly
(set-face-attribute 'default nil :font "Iosevka" :height 150)

;; UI CONFIGURATION
(setq inhibit-startup-message t)
(tool-bar-mode -1) ;Disable the toolbar
(menu-bar-mode -1);Disable the menu bar
(scroll-bar-mode -1);Disable the scrolbar
(tooltip-mode -1);Disable the toolbar



;; useful packages
(use-package dash
    :ensure t
    :config (eval-after-load "dash" '(dash-enable-font-lock))
    )

(use-package s
    :ensure t)

(use-package f
    :ensure t)



(fset 'yes-or-no-p 'y-or-n-p) ;; y or n

;; seperating the custom vars
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq-default indicate-empty-lines t)
(setq-default require-final-newline t)
(setq-default indent-tabs-mode t)

(setq history-length 25
	  show-paren-delay 0
	  debug-on-error t
	  warning-minimum-level :emergency
	  visible-bell t
	  display-line-numbers-type 'relative
	  display-line-numbers-width-start t
	  redisplay-dont-pause t
	  scroll-margin 1
	  scroll-step 1
	  scroll-preserve-screen-position 1
	  scroll-conservatively 1000
	  initial-scratch-message ";;Who The Fuck R U ??\n" ;; Uh, I know what Scratch is for
	  save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ediff-window-setup-function 'ediff-setup-windows-plain
      next-line-add-newlines t
	  switch-to-buffer-obey-display-actions t
	  list-matching-lines-default-context-lines 3
	  use-dialog-box nil
	  compilation-scroll-output t
	  xref-prompt-for-identifier nil
	  sentence-end-double-space nil
	  )

(setq create-lockfiles nil)
(savehist-mode 1)

(delete-selection-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
;; Bookmarks
(setq bookmark-save-flag t
	  bookmark-default-file "/home/caspeer/.cache/emacs/bookmarks")

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;(require 'server)
;;(require 'treesit)
