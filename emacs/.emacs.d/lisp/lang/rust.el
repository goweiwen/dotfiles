(use-package rust-mode
  :mode "\\.rs$"
  :config
  (setq rust-format-on-save t))

(use-package racer
  :after rust-mode
  :hook (rust-mode . racer-mode)
  :config
  (add-hook 'rust-mode-hook #'eldoc-mode)

  (setq racer-cmd (or (executable-find "racer")
                      (expand-file-name "racer/target/release/racer" +rust-src-dir))
        racer-rust-src-path (or (getenv "RUST_SRC_PATH")
                                (expand-file-name "rust/src/" +rust-src-dir)))

  (unless (file-exists-p racer-cmd)
    (warn "rust-mode: racer binary can't be found; auto-completion is disabled"))

  (set! :jump 'rust-mode :definition #'racer-find-definition))


(use-package company-racer
  :after racer
  :config (set! :company-backend 'rust-mode '(company-racer)))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :config (add-hook 'rust-mode-hook #'flycheck-mode))

(provide 'lang/rust)
