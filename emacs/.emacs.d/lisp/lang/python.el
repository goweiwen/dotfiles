(setq-default python-indent 4)
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

;; Anaconda mode
(use-package anaconda-mode
  :mode "\\.py$"
  :hook ((python-mode-hook anaconda-mode)
         (python-mode-hook anaconda-eldoc-mode)))

;; Autocompletion
(use-package company-anaconda
  :mode "\\.py$"
  :after anaconda-mode
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda))))

;; Disable Aggressive Indentation
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

;; Highlight Indent Guides
(use-package highlight-indent-guides
  :mode "\\.py$"
  :hook (python-mode-hook . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; Use yapf to format
(use-package yapfify
  :mode "\\.py$"
  :hook (python-mode-hook . yapf-mode))

(provide 'lang/python)
