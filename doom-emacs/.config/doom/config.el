;;; private/ww/config.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(menu-bar-mode 1)

(setq doom-font (font-spec :family "Fira Code" :size 14))

;;
;; Coq - Proof General
;;

;; (def-package! company-coq
;;   :config
;;   (add-hook 'coq-mode-hook #'company-coq-mode))

;; (setenv "PATH" (concat (getenv "PATH") "/Users/weiwen/.opam/4.05.0/bin"))
;; (setq exec-path (append exec-path '("/Users/weiwen/.opam/4.05.0/bin")))
