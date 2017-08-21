;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (defvar use-package-verbose t)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
  (setq use-package-always-ensure t))

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

;; Persistent Server
;; (use-package mac-pseudo-daemon
;;   :init
;;   (mac-pseudo-daemon-mode))
(require 'server)
(unless (server-running-p)
  (server-start))

;; Reload init.el
(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Edit init.el
(defun edit-init ()
  (interactive)
  (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el")))

;; Hot reloading init.el

(defun hot-reload-init ()
  (when (string= (buffer-file-name) "/Users/weiwen/.dotfiles/emacs/.emacs.d/init.el")
    (load-file (buffer-file-name))))

(add-hook 'after-save-hook 'hot-reload-init)

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
 '(variable-pitch ((t (:family "SFNS" :height 140)))))

;; Theme
(if window-system
    (use-package base16-theme
      :config
      (setq base16-theme-256-color-source 'base16-shell)
      (load-theme 'base16-ashes t)))

;; Tweak window chrome
(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

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

;; Hide a few minor modes
(diminish 'undo-tree-mode)
(diminish 'flycheck-mode)
(diminish 'projectile-mode)

;; Mode line
(use-package telephone-line
  :after evil
  :config
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-major-mode-segment))
          (nil    . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode))

;; Mouse scrolling
(setq mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)))

;; ;; Focus on current paragraph
;; (use-package focus
;;   :config
;;   (focus-mode))

;; evil-mode
;; ===

(use-package evil
  :init
  (evil-mode 1))

;; evil-surround
(use-package evil-surround
  :init
  (global-evil-surround-mode 1))

;; evil-commentary
(use-package evil-commentary
  :diminish evil-commentary-mode
  :init
  (evil-commentary-mode))

;; evil-snipe (vim-sneak)
(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :init
  (evil-snipe-mode 1)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  :config
  (setq evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'whole-visible
        evil-snipe-spillover-scope 'buffer))

;; Multiple cursors
(use-package evil-mc
  :init
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
  :init
  (evil-rsi-mode))

;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

;; evil-visual-mark-mode
(use-package evil-visual-mark-mode)

;; folding
(use-package vimish-fold
  :init
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
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.2))

;; Leader key is <SPC>
(defun hrs/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(defvar leader-map (make-sparse-keymap))
(define-key evil-normal-state-map [? ] leader-map)
(define-key evil-motion-state-map [? ] leader-map)
(define-key leader-map "!!" 'flycheck-list-errors)
(define-key leader-map "!n" 'flycheck-next-error)
(define-key leader-map "!p" 'flycheck-previous-error)
(define-key leader-map "bb" 'switch-to-buffer)
(define-key leader-map "bd" 'kill-this-buffer)
(define-key leader-map "fe" 'edit-init)
(define-key leader-map "ff" 'counsel-find-file)
(define-key leader-map "fr" 'counsel-recentf)
(define-key leader-map "fv" 'counsel-ag)
(define-key leader-map "fs" 'swiper)
(define-key leader-map "g" 'magit-status)
(define-key leader-map "h" 'counsel-describe-variable)
(define-key leader-map "pc" 'projectile-compile-project)
(define-key leader-map "pd" 'projectile-dired)
(define-key leader-map "pf" 'projectile-find-file)
(define-key leader-map "pp" 'projectile-switch-project)
(define-key leader-map "pr" 'projectile-recentf)
(define-key leader-map "pt" 'projectile-test)
(define-key leader-map "pv" 'projectile-ag)
(define-key leader-map "wd" 'delete-window)
(define-key leader-map "wh" 'split-window-verticaly)
(define-key leader-map "wv" 'split-window-horizontally)
(define-key leader-map [? ] 'counsel-M-x)
(define-key leader-map [C-p] 'projectile-switch-project)
(define-key leader-map [C-v] 'hrs/search-project-for-symbol-at-point)
(define-key leader-map [tab] 'mode-line-other-buffer)

;; Zoom
(use-package default-text-scale
  :config
  (global-set-key "\M-=" #'default-text-scale-increase)
  (global-set-key "\M--" #'default-text-scale-decrease))

;; Copy/Paste
(define-key global-map "\M-c" 'evil-yank)
(define-key global-map "\M-v" 'yank)

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
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;; Treat CamelCase as separate words
(global-subword-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Line numbers
(use-package nlinum-relative
  :config
  (setq nlinum-relative-redisplay-delay 0
        nlinum-relative-current-symbol ""
        nlinum-relative-offset 0
        nlinum-format "%d ")
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; Git Gutter
;; (use-package git-gutter
;;   :config
;;   (global-git-gutter-mode +1)
;;   (git-gutter:linum-setup))

;; Line wrapping
(setq-default truncate-lines t)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

;; ;; Minimap
;; (use-package minimap
;;   :init
;;   (minimap-mode)
;;   :config
;;   (setq minimap-window-location 'right
;;         minimap-width-fraction 0.1
;;         minimap-highlight-line nil))

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
(setq uniquify-buffer-name-style 'post-forward
      uniquify-min-dir-content 7)

;; Smooth scrolling
(use-package smooth-scrolling
  :init
  (smooth-scrolling-mode 1))

;; Autocompletion
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; Code highlighting
(use-package highlight-numbers
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))
(use-package highlight-quoted
  :init
  (add-hook 'prog-mode-hook #'highlight-quoted-mode))
(use-package highlight-defined
  :init
  (add-hook 'prog-mode-hook #'highlight-defined-mode))
(use-package highlight-operators
  :init
  (add-hook 'prog-mode-hook #'highlight-operators-mode))
(use-package highlight-escape-sequences
  :init
  (add-hook 'prog-mode-hook #'hes-mode))

;; Matching parenthesis
(show-paren-mode 1)
(setq-default show-paren-delay 0)

;; Linting with Flycheck
(use-package flycheck
  :diminish FlyCheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (use-package flycheck-pos-tip
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode))
  (use-package flycheck-color-mode-line
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; Snippets
(use-package yasnippet
  :diminish yas-global-mode yas-minor-mode
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

;; Projectile
;; ===

(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq projectile-switch-project-action 'projectile-dired
        projectile-use-git-grep t)
  (define-key evil-normal-state-map "\C-p" 'projectile-find-file))

(use-package counsel-projectile
  :after ivy
  :init
  (counsel-projectile-on))

(use-package ag)

;; Ivy
;; ===
(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  (add-to-list 'ivy-ignore-buffers "\\*magit"))

;; Dired
;; ===

(require 'dired)

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
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

;; Python
;; ===

(setq-default python-indent 2)
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

;; Anaconda mode
(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mpde-hook 'anaconda-eldoc-mode))

;; Autocompletion
(use-package company-anaconda
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda))))

;; Disable Aggressive Indentation
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

;; Highlight Indent Guides
(use-package highlight-indent-guides
  :init
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; Use yapf to format
(use-package yapfify
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

;; HTML
;; ===

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

;; CSS
;; ===
(setq-default css-indent-offset 2)
(use-package less-css-mode)
(use-package sass-mode)
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'sass-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'less-mode-hook 'rainbow-mode))

;; JavaScript
;; ===

(setq-default js-indent-level 2)

;; Prettier
(use-package prettier-js
  :ensure nil
  :quelpa (prettier-js
           :fetcher github
           :repo "prettier/prettier-emacs"
           :branch "prettier-js-prettify-region")
  :config
  (setq prettier-js-args '("--trailing-comma" "es5"
                           "--no-semi"
                           "--single-quote")))

;; Flycheck
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; React
(use-package rjsx-mode
  :mode ("\\.js\\'" . rjsx-mode)
  :init)
                                        ; (setq js2-mode-show-strict-warnings nil
                                        ;       js2-mode-show-parse-errors nil
                                        ;       js2-indent-level 2
                                        ;       js2-basic-offset 2
                                        ;       js2-strict-trailing-comma-warning nil
                                        ;       js2-strict-missing-semi-warning nil)

;; Vue
(use-package vue-mode
  :after 'prettier-js
  :init
  (defun prettier-vue ()
    (interactive)
    (let ((original (point)))
      (goto-char 0)
      (let* ((script-start (re-search-forward "<script>" nil t))
             (start (+ script-start 1))
             (script-end (re-search-forward "</script>" nil t))
             (end (- script-end 9)))
        (prettier-js--prettify start end)
        (goto-char original)
        (vue-mode-reparse))))
  (add-hook 'vue-mode-hook
            (lambda ()
              (prettier-js-mode)
              (add-hook 'before-save-hook 'prettier-vue nil 'local))))

;; Org
;; ===

(use-package org
  :ensure org-plus-contrib
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (org-bullets-mode t)))
  :config
  (setq org-ellipsis " ▼"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-html-postamble nil
        org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-startup-with-latex-preview t
        org-startup-with-inline-images t))
;; Export
(require 'ox-md)
(require 'ox-beamer)

;; Other Languages
;; ===

(use-package coffee-mode)
(use-package lua-mode)
(use-package elm-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package haskell-mode)
(use-package swift-mode)

;; Writing
;; ===

;; Darkroom
(use-package darkroom)

;; Use variable pitch for text modes
(use-package mixed-pitch
  :init
  (add-hook 'text-mode-hook 'mixed-pitch-mode))

;; Proselint
(require 'flycheck)
(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))
(add-to-list 'flycheck-checkers 'proselint)

(add-hook 'text-mode-hook #'flycheck-mode)

;; Automatically generated

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company-anaconda py-yapf ag))))

;;; init.el ends here
