(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :hook
  (go-mode . (lambda () (add-hook 'before-save-hook 'gofmt-before-save)))
  :config
  (setq gofmt-command "goimports"))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(use-package go-guru
  :after go-mode
  :commands (go-guru-describe go-guru-freevars go-guru-implements go-guru-peers
             go-guru-referrers go-guru-definition go-guru-pointsto
             go-guru-callstack go-guru-whicherrs go-guru-callers go-guru-callees
             go-guru-expand-region)
  :config
  (unless (executable-find "guru")
    (warn "go-mode: couldn't find guru, refactoring commands won't work")))

(use-package company-go
  :after go-mode)

(provide 'lang/go)
