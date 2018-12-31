(use-package lean-mode
  :mode ("\\.lean$" . lean-mode))

(use-package company-lean
  :after lean-mode)

;; (use-package helm-lean
;;   :after lean-mode)

(provide 'lang/lean)
