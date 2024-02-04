;; Package System Setup
(require 'package)
(setq package-archives '(
                         ("melpa" .  "http://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ))

(package-initialize)

(unless package-archive-contents
    (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)


;; Runtime Performance

;; helps to keep .emacs.d clean
(make-directory (expand-file-name ".tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name ".tmp/auto-saves/sessions/" user-emacs-directory)
	  auto-save-file-name-transforms `((".*" ,(expand-file-name ".tmp/auto-saves/" user-emacs-directory) t)))
(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/" user-emacs-directory))))



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


(setq history-length 25
      debug-on-error t
	  scroll-margin 6
	  scroll-step 6
	  scroll-conservatively 999
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

(require 'server)
(require 'treesit)





