(setq-default css-indent-offset 2)
(use-package less-css-mode
  :mode "\\.less$"
  :hook less-mode-hook)
(use-package sass-mode
  :mode "\\.s[ac]ss$"
  :hook sass-mode-hook)
(use-package sws-mode
  :mode "\\.stylus$"
  :hook stylus-mode-hook)
(use-package rainbow-mode
  :mode "\\.css$"
  :hook ((css-mode-hook . rainbow-mode)
         (sass-mode-hook . rainbow-mode)
         (html-mode-hook . rainbow-mode)
         (less-mode-hook . rainbow-mode))
  :diminish rainbow-mode)

(provide 'lang/css)
