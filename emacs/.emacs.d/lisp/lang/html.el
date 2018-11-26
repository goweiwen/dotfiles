(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  (setq web-mode-enable-css-colorization t)
  (setq-default css-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-attr-indent-offset 2))

;; Preprocessors
(use-package pug-mode
  :mode "\\.pug$")

(provide 'lang/html)
