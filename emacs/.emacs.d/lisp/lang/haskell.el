(use-package haskell-mode
  :mode "\\.hs$"
  :mode ("\\.ghci$" . ghci-script-mode)
  :mode ("\\.cabal$" . haskell-cabal-mode)
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :config
  (load "haskell-mode-autoloads" nil t)

  (push ".hi" completion-ignored-extensions))

(use-package intero
  :hook (haskell-mode . intero-mode)
  :config
  (unless (executable-find "stack")
    (warn "haskell-mode: couldn't find stack, disabling intero")
    (remove-hook 'haskell-mode-hook #'intero-mode))

  (add-hook 'intero-mode-hook #'(flycheck-mode eldoc-mode)))

(use-package hindent
  :hook (haskell-mode . hindent-mode))

(provide 'lang/haskell)
