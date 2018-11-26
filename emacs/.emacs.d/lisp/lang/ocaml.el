(use-package tuareg
  :mode ("\\.ml[4ilpy]?$" . tuareg-mode))
(use-package merlin
  :after tuareg
  :hook (tuareg-mode . merlin-mode))

(provide 'lang/ocaml)
