;; Package System Setup

(setq
 package-archives '(("melpa" . "http://melpa.org/packages/")
					("gnu"   . "https://elpa.gnu.org/packages/"))
 )


(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; Font Familly
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 130)

;; 256 colors support for fbterm.
(when (string-equal (getenv "TERM") "fbterm")
  (load "term/xterm")
  (defun terminal-init-fbterm ()
    "Terminal initialization function for linux fbterm."
    (unless (terminal-coding-system)
      (set-terminal-coding-system 'utf-8-unix))

    ;; It can't really display underlines.
    (tty-no-underline)

    (ignore-errors (when gpm-mouse-mode (require 't-mouse) (gpm-mouse-enable)))

    (xterm-register-default-colors xterm-standard-colors))
  (terminal-init-fbterm))


;; UI CONFIGURATION
(setq inhibit-startup-message t
	  use-dialog-box nil
	  warning-minimum-level :emergency
	  display-line-numbers-type 'relative
	  initial-scratch-message ";;caspeer"
	  ring-bell-function 'ignore
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
 package-native-compile nil
 next-line-add-newlines t
 load-prefer-newer t
 require-final-newline t
 save-interprogram-paste-before-kill t
 switch-to-buffer-obey-display-actions nil
 list-matching-lines-default-context-lines 3
 sentence-end-double-space t
 compilation-scroll-output t
 set-mark-command-repeat-pop t
 backward-delete-char-untabify-method 'hungry
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
 bookmark-default-file (expand-file-name "bookmarks/" user-emacs-directory))

(defun disable-tabs ()
	(interactive)
	(setq indent-tabs-mode nil))

(defun enable-tabs  ()
	;;(local-set-key (kbd "TAB") 'tab-to-tab-stop)
	(interactive)
	(local-set-key (kbd "TAB") 'indent-for-tab-command)
	(setq indent-tabs-mode t)
	(setq tab-width custom-tab-width))


(add-hook 'prog-mode-hook 'enable-tabs)

;; Isearch
(setq
 isearch-lazy-count t
 lazy-count-prefix-format "(%s/%s) "
 lazy-count-suffix-format nil
 )

(use-package doom-modeline
	:ensure t
	:init (doom-modeline-mode 1))

;; Common Lisp
(setq inferior-lisp-program "clisp")

(global-completion-preview-mode 1)

(use-package whitespace
			 :bind ("C-c t w" . whitespace-mode)
			 :init
			 (setq whitespace-line-column nil
				   whitespace-display-mappings '((space-mark 32 [183] [46])
												 (newline-mark 10 [9166 10])
												 (tab-mark 9 [187 9] [92 9])))
			 :config
			 (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))


;;TODO(caspeer): bind more functions from this useful package
(use-package expand-region
	:bind
	("C-=" . 'er/expand-region)
	)

(use-package winner
			 :init
			 (winner-mode 1)
			 :bind (("C-c u" . winner-undo)
					("C-c r" . winner-redo))
			 )

;; Hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


(defface font-lock-note-face
	'((t :foreground "green" :weight bold))
	"Face for my special keywords in font-lock."
	)

(defvar font-lock-note-face 'font-lock-note-face
	"My note face variable"
	)
(defface my-font-lock-warning-face
	'((t :foreground "red" :weight bold))
	"Face for my special keywords in font-lock."
	)

(defvar my-font-lock-warning-face 'my-font-lock-warning-face
	"My warning face variable"
	)
(add-hook 'prog-mode-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(
									  ("\\<\\(FIX\\|TODO\\|BUG\\)" 1 my-font-lock-warning-face prepend)
									  ("\\<\\(NOTE\\|HACK\\)" 1 font-lock-note-face prepend)
									  ))))

(use-package prescient
	:ensure t
	:config
	(prescient-persist-mode)
	)

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

(use-package recentf
	:config
	(setq recentf-max-menu-items 20
		  recentf-max-saved-items 100
		  recentf-keep '(file-readable-p file-exists-p)
		  )
	(recentf-mode t)
	:bind ("C-c f r" . 'recentf)
	)

;; Global HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Dired ------------------------
(use-package dired
	:hook ('dired-mode-hook 'auto-revert-mode)
	:bind (:map  dired-mode-map
				 ("-" . 'dired-up-directory)
				 ("1" . 'dired-do-shell-command)
				 ("/ c" . 'dired-mark-files-containing-regexp)
				 ("/ r" . 'dired-mark-files-regexp)
				 ("/ e" . 'dired-mark-executables)
				 ("/ d" . 'dired-mark-directories)
				 ("/ l" . 'dired-mark-symlinks)
				 ("/ s" . 'dired-mark-subdir-files)
				 ("/ t" . 'dired-toggle-marks)
				 ("c" . 'dired-do-copy)
				 ("C" . 'dired-do-compress-to)
				 )
	:custom ((dired-listing-switches "-aghlt ")) ;;--group-directories-first
	:config
	(setq
	 dired-dwim-target t
	 dired-kill-when-opening-new-dired-buffer t
	 )
	)

;;  Registers
(setq register-preview-delay 0)

;; KBD MACROS
(define-prefix-command 'caspeer/kbd-macros)
(global-set-key (kbd "C-c m") 'caspeer/kbd-macros)

;; mini-buffer completion
(selectrum-mode +1)
	;; (setq completion-styles '(orderless)
	;; 	  selectrum-prescient-enable-filtering nil
	;; 	  )
(selectrum-prescient-mode)

;; HIPPIE
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
		                                 try-expand-dabbrev
		                                 try-expand-all-abbrevs
		                                 try-expand-list try-expand-line
		                                 try-expand-dabbrev-from-kill
		                                 try-expand-dabbrev-all-buffers
		                                 try-complete-lisp-symbol-partially
		                                 try-complete-lisp-symbol))

(setq hippie-expand-verbose nil)
(setf completion-styles '(basic flex))

(use-package flyspell-mode
	:hook ((org-mode . markdown-mode) . flyspell-mode)
	:bind
	(:map flyspell-mode
		  ("C-," . 'flyspell-goto-next-error)
		  ("C-;" . 'flyspell-auto-correct-word)
		  ("C-M-;" . 'ispell-region)
		  ("C-$" . 'ispell-word)
		  ("C-M-#" . 'ispell-complete-word)
		  )
	)

;; Snippets
(use-package yasnippet-snippets
	:after yasnippet
	:ensure t)

(use-package yasnippet
	:ensure t
	:config
	(setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/snippets")))
	:hook ((text-mode
			prog-mode
			conf-mode
			snippet-mode) . yas-minor-mode-on )
	)

(defmacro git-source (user repo &optional branch releases tags)
  "Expand into a list of Elfeed feed entries for GitHub repo."
  (let* ((base-url (format "https://github.com/%s/%s" user repo))
         (feeds
          (list
           (if branch
               (format "%s/commits/%s.atom" base-url branch)
             (format "%s/commits.atom" base-url))))
         (feeds
          (append feeds
                  (when releases (list (format "%s/releases.atom" base-url)))
                  (when tags     (list (format "%s/tags.atom" base-url))))))
    ;; Return feed entries, each with tags like 'repo' and the repo name
    `(list
      ,@(mapcar (lambda (url)
                  `(list ,url 'repo ',(intern repo)))
                feeds))))


(use-package elfeed
	:config
	(setq elfeed-search-filter "@1-months-ago +unread -junk -repo")
	(setq elfeed-feeds '(
						 ("https://www.computerenhance.com/feed" casey)
						 ("https://nullprogram.com/feed/" blog null)
						 ("https://blog.cryptographyengineering.com/feed/" blog)
						 ("https://www.redblobgames.com/blog/posts.xml" blog)
						 ("https://utcc.utoronto.ca/~cks/space/blog/?atom" blog dev)
						 ("https://lemire.me/blog/feed/" dev blog)
						 ("https://danluu.com/atom.xml" dev blog)
						 ("https://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
						 ("https://fabiensanglard.net/rss.xml" blog dev)
						 ("https://flak.tedunangst.com/rss" dev blog)
						 ("https://nrk.neocities.org/rss.xml" blog dev)
						 ("https://www.smbc-comics.com/comic/rss" comic)
						 ("https://xkcd.com/atom.xml" comic)
						 ;; Youtube
						 ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyvk_lo0codsbB8oWL6xUBA" al-sabeel)
						 ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrqM0Ym_NbK1fqeQG2VIohg" tsoding)
						 ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJXa3_WNNmIpewOtCHf3B0g" laurie )
						 ("https://www.youtube.com/feeds/videos.xml?channel_id=UCS0N5baNlQWJCUrhCEo8WlA" beneater)
						 ("https://www.youtube.com/feeds/videos.xml?channel_id=UCW6MNdOsqv2E9AjQkv9we7A" pwnf)
						 ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9J9u3apteD0EuFjzRpt71w" wookash-pod)
						 ("https://www.youtube.com/feeds/videos.xml?channel_id=UCi8C7TNs2ohrc6hnRQ5Sn2w" programmer-are-also-humans)
						 ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA" computerphile)
						 ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUyeluBRhGPCW4rPe_UvBZQ" prime)
						 ;; Repos
						 ))
	(setq elfeed-feeds (append elfeed-feeds (git-source "tsoding" "nob.h")))
	)

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :entry-link "youtube\\.com/shorts"
                              :add 'junk
                              :remove 'unread))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))

(global-set-key (kbd "C-c o") 'elfeed)

(use-package move-dup
	:bind (("M-p"   . move-dup-move-lines-up)
           ("C-M-p" . move-dup-duplicate-up)
           ("M-n"   . move-dup-move-lines-down)
           ("C-M-n" . move-dup-duplicate-down)))

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
(global-set-key (kbd "M-m") 'recompile)
(setq c-basic-offset custom-tab-width)
(add-hook 'c-mode-hook
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
(global-set-key (kbd "C-c a"	) 'async-shell-command)
;(global-set-key (kbd "C-c m"	) 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-x k"	) 'kill-current-buffer)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
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

(use-package haskell-mode
	:config
	(disable-tabs)
	(setq
	 haskell-indent-spaces 4
	 )
	)

(use-package typst-ts-mode
	:vc (:url "https://codeberg.org/meow_king/typst-ts-mode.git"))

(setq
 treesit-language-source-alist '(
								 (typst "https://github.com/uben0/tree-sitter-typst")
								 )
 )

(use-package envrc
	:ensure t
	:when (executable-find "direnv")
	:bind-keymap ("C-c e v" . envrc-command-map)
	:hook (after-init . envrc-global-mode))

(setq envrc-mode-line-function #'envrc-mode-line-format)

(use-package ediff
	:config
	(setq ediff-split-window-function 'split-window-horizontally
		  ediff-window-setup-function 'ediff-setup-windows-plain)
	)

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

;; remap C-c C-b to Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-unset-key (kbd "C-x C-c"))
(setq ibuffer-expert t)

(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
			   "Discard all themes before loading new."
			   (mapc #'disable-theme custom-enabled-themes))

(load-theme 'modus-operandi-tinted)
