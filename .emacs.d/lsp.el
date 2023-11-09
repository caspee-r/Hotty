

;; Lsp-Mode
(use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook (lsp-mode . lsp-headerline-breadcrumb-mode) 
    :init
    (setq lsp-keymap-prefix "C-c l")
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (setq lsp-idle-delay 0.500)

    :bind
    (:map lsp-mode-map
	  ("<Tab>" . company-indent-or-complete-common))
    :config
    (lsp-enable-which-key-integration t))

(use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
    :after lsp)


;; (use-package flycheck
;;     :ensure t
;;     :after lsp
;;     :config
;;     (flycheck-mode 1)
;;     )
