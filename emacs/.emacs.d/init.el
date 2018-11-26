;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))
(setq use-package-always-ensure t)

;; quelpa-use-package
(setq-default quelpa-update-melpa-p nil)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))
(use-package quelpa-use-package)

;; Personal Information
(setq user-full-name "Goh Wei Wen"
      user-mail-address "goweiwen@gmail.com")

;; Secrets
(if (file-exists-p "secrets.el")
  (load-file "secrets.el"))

;; Persistent Server
;; (use-package mac-pseudo-daemon
;;   :init
;;   (mac-pseudo-daemon-mode))
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; Reload init.el
(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Edit init.el
(defun edit-init ()
  (interactive)
  (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el")))

;; Edit todo.org
(defun edit-todo ()
  (interactive)
  (switch-to-buffer (find-file-noselect "~/Notes/todo.org")))

;; ;; Hot reloading init.el
;; (defun hot-reload-init ()
;;   (when (string= (buffer-file-name) "/Users/weiwen/.dotfiles/emacs/.emacs.d/init.el")
;;     (load-file (buffer-file-name))))
;; (add-hook 'after-save-hook 'hot-reload-init)

;; Backup directory
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Use case-insensitive file search
(setq read-file-name-completion-ignore-case t)

;; Auto Revert
(diminish 'auto-revert-mode)
(global-auto-revert-mode 1)

;; Save History
(savehist-mode 1)

;; Save Session
; (desktop-save-mode 1)

;; Recentf
(require 'recentf)
(run-at-time (* 5 60) nil
             (lambda ()
               (let ((inhibit-message t))
                 (recentf-save-list))))

;; Appearance
;; ===

;; Font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :height 140))))
 '(fixed-pitch ((t (:family "Menlo" :height 140))))
 '(proof-locked-face ((nil (:background "#003300"))))
 '(proof-queue-face ((nil (:background "#116611"))))
 '(variable-pitch ((t (:family "SFNS" :height 140)))))

;; Theme
(when (display-graphic-p)
  (use-package solarized-theme
    :config
    (load-theme 'solarized-light t)))

;; Tweak window chrome
(tool-bar-mode -1)
(unless (eq system-type 'darwin) menu-bar-mode -1)
(when window-system (scroll-bar-mode -1))

;; Blend fringe background
(set-face-attribute 'fringe nil :background nil)

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'official
        dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5))))

;; Frame title
(setq frame-title-format '((:eval (projectile-project-name))))

;; Undo window changes
(winner-mode)

;; Hide a few minor modes
(diminish 'undo-tree-mode)

;; Mode line
(use-package telephone-line
  :after evil
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-nil
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-nil
        telephone-line-secondary-right-separator 'telephone-line-nil)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment))
          (nil    . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-major-mode-segment))
          (accent . (telephone-line-misc-info-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode))

;; Quality of Life
(setq vc-follow-symlinks t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Terminal stuff
(xterm-mouse-mode 1)
(normal-erase-is-backspace-mode 1)

;; ;; Emacs is not a window manager
;; (use-package frames-only-mode)

;; evil-mode
;; ===

(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil
        evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; evil-unimpaired
(use-package evil-unimpaired
  :quelpa (evil-unimpaired :fetcher github :repo "zmaas/evil-unimpaired")
  :config
  (add-hook 'evil-mode-hook 'evil-unimpaired-mode))

;; evil-commentary
(use-package evil-commentary
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

;; evil-snipe (vim-sneak)
(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :config
  (evil-snipe-mode 1)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (setq evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'whole-visible
        evil-snipe-spillover-scope 'buffer))

;; Multiple cursors
(use-package evil-mc
  :diminish
  :config
  (global-evil-mc-mode 1))

;; evil-arg
;; Motion for ,; delimited arguments
(use-package evil-args
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg))

;; evil-rsi
;; Emacs bindings in evil-mode
(use-package evil-rsi
  :diminish evil-rsi-mode
  :config
  (evil-rsi-mode))

;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

;; evil-visual-mark-mode
(use-package evil-visual-mark-mode)

;; folding
(use-package vimish-fold
  :config
  (vimish-fold-global-mode 1))

;; Visual sticky shift indentation

(define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)
(define-key evil-visual-state-map [tab] 'djoyner/evil-shift-right-visual)
(define-key evil-visual-state-map [S-tab] 'djoyner/evil-shift-left-visual)
(defun djoyner/evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun djoyner/evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;; Keybindings
;; ===

;; which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.2))

;; Buffer list
(define-key evil-motion-state-map [?_] 'buffer-menu)
(define-key evil-normal-state-map [?_] 'buffer-menu)
(define-key Buffer-menu-mode-map [?_]
  (lambda () (interactive) (switch-to-buffer nil) (ivy-switch-buffer)))
(define-key evil-normal-state-map "]b" 'switch-to-next-buffer)
(define-key evil-normal-state-map "[b" 'switch-to-prev-buffer)
(define-key evil-motion-state-map "]b" 'switch-to-next-buffer)
(define-key evil-motion-state-map "[b" 'switch-to-prev-buffer)

;; Leader key is <SPC>
(defvar leader-map (make-sparse-keymap))

(define-key evil-motion-state-map [? ] leader-map)
(define-key evil-normal-state-map [? ] leader-map)
(define-key leader-map "[" 'switch-to-prev-buffer)
(define-key leader-map "]" 'switch-to-next-buffer)
(define-key leader-map "bb" 'switch-to-buffer)
(define-key leader-map "bd" 'kill-this-buffer)
(define-key leader-map "c" 'org-capture)
(define-key leader-map "fe" 'edit-init)
(define-key leader-map "ff" 'counsel-find-file)
(define-key leader-map "fp" 'counsel-git)
(define-key leader-map "fr" 'counsel-recentf)
(define-key leader-map "fs" 'swiper)
(define-key leader-map "ft" 'edit-todo)
(define-key leader-map "fg" 'counsel-rg)
(define-key leader-map "g" 'magit-status)
(define-key leader-map "h" 'counsel-describe-variable)
(define-key leader-map "ot" (lambda () (interactive) (eshell 'N)))
(define-key leader-map "pb" 'counsel-projectile-switch-to-buffer)
(define-key leader-map "pc" 'projectile-compile-project)
(define-key leader-map "pd" 'projectile-dired)
(define-key leader-map "pf" 'projectile-find-file)
(define-key leader-map "pp" 'projectile-switch-project)
(define-key leader-map "pr" 'projectile-recentf)
(define-key leader-map "pt" 'projectile-test)
(define-key leader-map "pv" 'projectile-ag)
(define-key leader-map [?:] 'counsel-M-x)
(define-key leader-map [?\t] 'mode-line-other-buffer)
(define-key leader-map (kbd "C-p") 'projectile-switch-project)
(define-key leader-map (kbd "C-v") 'hrs/search-project-for-symbol-at-point)
(define-key leader-map [escape] 'evil-ex-nohighlight)

;; Zoom
(use-package default-text-scale
  :config
  (global-set-key "\M-0" 'default-text-scale-reset)
  (global-set-key "\M-=" 'default-text-scale-increase)
  (global-set-key "\M--" 'default-text-scale-decrease))

;; Switch buffers
(define-key global-map [C-tab] 'next-buffer)
(define-key global-map [C-S-tab] 'previous-buffer)

;; Editor
;; ===

(setq sentence-end-double-space nil)

;; Indentation
(setq-default tab-width 2
              indent-tabs-mode nil)

;; Aggressive Indentation
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook (prog-mode-hook aggressive-indent-mode))

;; Treat _ as part of a word-object
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; Highlight current line
(global-hl-line-mode 1)

;; ;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Git Gutter
(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode +1))

;; Line wrapping
(setq-default truncate-lines t)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

;; Fancy lambdas
(global-prettify-symbols-mode t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Whitespace-mode
(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face
                           traiiling
                           space-before-tab
                           space-after-tab
                           lines-tail
                           empty))
  (add-hook 'prog-mode-hook 'whitespace-mode))

;; Path information in mode-line
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-min-dir-content 7)

;; Smooth scrolling
(diminish 'pixel-scroll-mode)
(pixel-scroll-mode)

;; Autocompletion
(use-package company
  :diminish
  :hook (after-init-hook global-company-mode)
  :config
  (setq company-idle-delay 0.2))

;; Code highlighting
(use-package highlight-numbers
  :hook (prog-mode-hook highlight-numbers-mode))
(use-package highlight-quoted
  :hook (prog-mode-hook highlight-quoted-mode))
(use-package highlight-defined
  :hook (prog-mode-hook highlight-defined-mode))
(use-package highlight-operators
  :hook (prog-mode-hook highlight-operators-mode))
(use-package highlight-escape-sequences
  :hook (prog-mode-hook hes-mode))

;; Matching parenthesis
(show-paren-mode 1)
(setq-default show-paren-delay 0)

;; Linting with Flycheck
(use-package flycheck
  :after evil
  :diminish
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.2
        flycheck-idle-change-delay 0.2)
  (define-key evil-normal-state-map "!" nil)
  (define-key evil-normal-state-map "!!" 'flycheck-list-errors)
  (define-key evil-normal-state-map "!n" 'flycheck-next-error)
  (define-key evil-normal-state-map "!p" 'flycheck-previous-error)
  (define-key evil-normal-state-map "]!" 'flycheck-next-error)
  (define-key evil-normal-state-map "[!" 'flycheck-previous-error)
  (define-key evil-motion-state-map "!" nil)
  (define-key evil-motion-state-map "!!" 'flycheck-list-errors)
  (define-key evil-motion-state-map "!n" 'flycheck-next-error)
  (define-key evil-motion-state-map "!p" 'flycheck-previous-error)
  (define-key evil-motion-state-map "]!" 'flycheck-next-error)
  (define-key evil-motion-state-map "[!" 'flycheck-previous-error))
(use-package flycheck-pos-tip
  :after flycheck
  :hook (flycheck-mode-hook flycheck-pos-tip-mode))
(use-package flycheck-color-mode-line
  :after flycheck
  :hook (flycheck-mode-hook flycheck-color-mode-line-mode))

;; Snippets
(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :hook (after-init-hook yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

;; Projectile
;; ===

(use-package projectile
  :diminish
  :config
  (projectile-mode)
  (setq projectile-switch-project-action 'projectile-find-file
        projectile-use-git-grep t
        projectile-completion-system 'ivy)
  (define-key evil-normal-state-map "\C-p" 'projectile-find-file))

(use-package counsel-projectile
  :after ivy)

;; Ivy
;; ===

(use-package ivy
  :diminish ivy-mode
  :bind
  ("C-S-s" . counsel-rg)
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (add-to-list 'ivy-ignore-buffers "\\*dashboard\\*")
  (add-to-list 'ivy-ignore-buffers "\\*dired\\*")
  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  (add-to-list 'ivy-ignore-buffers "\\magit"))

;; Swiper
;; ===

(use-package swiper
  :bind
  ("C-s" . swiper))

;; Eshell
;; ===
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Dired
;; ===

;; (require 'dired)
(use-package dired-single
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode)))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-single-use-magic-buffer t)
  (define-key evil-normal-state-map [?-]
    (lambda () (interactive) (dired-single-magic-buffer default-directory)))
  (define-key evil-motion-state-map [?-]
    (lambda () (interactive) (dired-single-magic-buffer default-directory)))
  (define-key dired-mode-map [? ] leader-map)
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map [?^] 'dired-single-up-directory)
  (define-key dired-mode-map [?-] 'dired-single-up-directory))

;; macOS GNU ls
(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls)
      (setq insert-directory-program gls)))

;; find-dired
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;; Sort directories first
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; Version Control
;; ===

;; git interface
(use-package magit
  :config
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (setq magit-auto-revert-mode nil))
(use-package evil-magit)

;; Highlight uncommitted changes
(use-package diff-hl
  :hook ((prog-mode-hook turn-on-diff-hl-mode)
         (vc-dir-mode-hook turn-on-diff-hl-mode)))

;; Languages
;; ===

(require 'lang/coq)
(require 'lang/elm)
(require 'lang/go)
(require 'lang/haskell)
(require 'lang/lua)
(require 'lang/moonscript)
(require 'lang/ocaml)
(require 'lang/python)
(require 'lang/rust)
(require 'lang/swift)

(require 'lang/json)
(require 'lang/yaml)

(require 'lang/coffee)
(require 'lang/css)
(require 'lang/html)
(require 'lang/js)
(require 'lang/vue)

(require 'lang/org)

;; Writing
;; ===

;; Use variable pitch for text modes
(use-package mixed-pitch
  :hook ('org-mode-hook 'mixed-pitch-mode))

;; Automatically generated

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote ())))

;;; init.el ends here
