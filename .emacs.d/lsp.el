

;; Lsp-Mode
(use-package lsp-mode
    :commands (lsp lsp-deferred)
	:init
    (setq lsp-keymap-prefix "C-c l")
    ;; (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (setq lsp-idle-delay 0.500)

    :bind
    (:map lsp-mode-map
		  ("<Tab>" . company-indent-or-complete-common))
    :config
    (lsp-enable-which-key-integration t)
	(setq lsp-auto-guess-root t)
	(setq lsp-log-io nil)
	(setq lsp-restart 'auto-restart)
	(setq lsp-enable-symbol-highlighting nil)
	(setq lsp-enable-on-type-formatting nil)
	(setq lsp-signature-auto-activate nil)
	(setq lsp-signature-render-documentation nil)
	(setq lsp-eldoc-hook nil)
	(setq lsp-modeline-code-actions-enable nil)
	(setq lsp-modeline-diagnostics-enable nil)
	(setq lsp-headerline-breadcrumb-enable nil)
	(setq lsp-semantic-tokens-enable nil)
	(setq lsp-enable-folding nil)
	(setq lsp-enable-imenu nil)
	(setq lsp-enable-snippet nil)
  	)

(use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
	:commands lsp-ui-mode
    :config
	(setq lsp-ui-doc-enable nil)
	(setq lsp-ui-doc-header t)
	(setq lsp-ui-doc-include-signature t)
	(setq lsp-ui-doc-border (face-foreground 'default))
	(setq lsp-ui-sideline-show-code-actions t)
	(setq lsp-ui-sideline-delay 0.05)
	)

(use-package lsp-treemacs
    :after lsp)


;; (use-package flycheck
;;     :ensure t
;;     :after lsp
;;     :config
;;     (flycheck-mode 1)
;;     )
