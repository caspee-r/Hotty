
; Evil Mode
(setq evil-want-keybinding nil)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun evil-scroll-up-center ()
    "scroll down half-screen and center the  cursor line"
    (interactive)
    (evil-scroll-up 0 )
    (evil-scroll-line-to-center (line-number-at-pos)))


(defun evil-scroll-down-center ()
    "scroll down half-screen and center the  cursor line"
    (interactive)
    (evil-scroll-down 0 )
    (evil-scroll-line-to-center (line-number-at-pos)))

(use-package evil
    :init
    (setq evil-undo-system 'undo-fu)
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump t)
    (setq evil-want-minibuffer nil)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
    (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down-center)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up-center)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
	
    )

(use-package evil-collection
    :ensure t
    :after evil
    :config
    (evil-collection-init))

(use-package undo-fu                    ;
    :ensure t)

