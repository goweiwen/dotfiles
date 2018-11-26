(use-package company-coq
  :mode ("\\.v$" . coq-mode)
  :hook ('coq-mode-hook . 'company-coq-mode)
  :bind (:map company-coq-map
              ("M-j" . proof-assert-next-command-interactive)
              ("M-k" . proof-undo-last-successful-command)
              ("M-<return>" . proof-goto-point))
  :custom-face
  (proof-locked-face ((nil (:background "#003300"))))
  (proof-queue-face ((nil (:background "#116611"))))
  :config

  (add-hook 'coq-mode-hook #'company-coq-mode)

  ;; Appearance

  ;; Use Coq 8.6 (for CS3234)
  (setenv "PATH" (concat (getenv "PATH") "/Users/weiwen/.opam/4.05.0/bin"))
  (setq exec-path (append exec-path '("/Users/weiwen/.opam/4.05.0/bin")))

  ;; Prettify symbols
  (company-coq-features/prettify-symbols nil)
  (add-hook 'coq-mode-hook
            (lambda ()
              (setq-local prettify-symbols-alist
                          '(("|-" . 8866)
                            ("||" . 8214)
                            ("/\\" . 8743)
                            ("\\/" . 8744)
                            ("->" . 8594)
                            ("<-" . 8592)
                            ("<->" . 8596)
                            ("=>" . 8658)
                            ("<=" . 8804)
                            (">=" . 8805)
                            ("True" . 8868)
                            ("False" . 8869)
                            ("fun" . 955)
                            ("forall" . 8704)
                            ("exists" . 8707)
                            ("nat" . 8469)
                            ("Prop" . 8473)
                            ("Real" . 8477)
                            ("bool" . 120121)
                            (">->" . 8611)
                            ("-->" . 10230)
                            ("<--" . 10229)
                            ("<-->" . 10231)
                            ("==>" . 10233)
                            ("<==" . 10232)
                            ("~~>" . 10239)
                            ("<~~" . 11059))))))

(provide 'lang/coq)
