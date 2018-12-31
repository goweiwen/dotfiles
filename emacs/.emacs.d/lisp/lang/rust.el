(use-package rust-mode
  :mode "\\.rs$"
  :config
  (setq rust-format-on-save t))

(use-package racer
  :after rust-mode
  :hook (rust-mode . racer-mode)
  :config
  (add-hook 'rust-mode-hook #'eldoc-mode)

  (unless (file-exists-p racer-cmd)
    (warn "rust-mode: racer binary can't be found; auto-completion is disabled")))


(use-package company-racer
  :after racer)

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :config (add-hook 'rust-mode-hook #'flycheck-mode))

(provide 'lang/rust)
