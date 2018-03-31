;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)
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
(load "~/.emacs.d/secrets.el")

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
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
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
 '(default ((t (:family "Fira Code" :height 140))))
 '(fixed-pitch ((t (:family "Fira Code" :height 140))))
 '(variable-pitch ((t (:family "SFNS" :height 140)))))

;; Theme
(if window-system
    (use-package base16-theme
      :config
      (setq base16-theme-256-color-source 'base16-shell)
      (load-theme 'base16-ashes t)))

;; Tweak window chrome
(tool-bar-mode 0)
(menu-bar-mode 1)
(when window-system
  (scroll-bar-mode -1))

;; Blend fringe background
(set-face-attribute 'fringe nil :background nil)

;; ;; Dashboard
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner 'official
;;         dashboard-items '((recents . 5)
;;                           (bookmarks . 5)
;;                           (projects . 5)
;;                           (agenda))))

;; Frame title
(setq frame-title-format '((:eval (projectile-project-name))))

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
          (accent . (telephone-line-minor-mode-segment
                     telephone-line-misc-info-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode))

;; Terminal stuff
(xterm-mouse-mode 1)
(normal-erase-is-backspace-mode 1)

;; Emacs is not a window manager
(use-package frames-only-mode)

;; ;; Mouse scrolling
;; (setq mac-mouse-wheel-smooth-scroll nil
;;       mouse-wheel-follow-mouse t
;;       mouse-wheel-progressive-speed nil
;;       mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)))

;; ;; Focus on current paragraph
;; (use-package focus
;;   :config
;;   (focus-mode))

;; evil-mode
;; ===

(use-package evil
  :config
  (evil-mode 1))

;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

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

;; Leader key is <SPC>
(defun hrs/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(defvar leader-map (make-sparse-keymap))
(define-key evil-normal-state-map [? ] leader-map)
(define-key evil-motion-state-map [? ] leader-map)
(require 'dired)
(define-key dired-mode-map [? ] leader-map)
(define-key leader-map "!!" 'flycheck-list-errors)
(define-key leader-map "!n" 'flycheck-next-error)
(define-key leader-map "!p" 'flycheck-previous-error)
(define-key leader-map "bb" 'switch-to-buffer)
(define-key leader-map "bd" 'kill-this-buffer)
(define-key leader-map "fe" 'edit-init)
(define-key leader-map "ft" 'edit-todo)
(define-key leader-map "ff" 'counsel-find-file)
(define-key leader-map "fp" 'counsel-git)
(define-key leader-map "fr" 'counsel-recentf)
(define-key leader-map "fs" 'swiper)
(define-key leader-map "fv" 'counsel-ag)
(define-key leader-map "g" 'magit-status)
(define-key leader-map "h" 'counsel-describe-variable)
(define-key leader-map "oa" 'org-agenda)
(define-key leader-map "oc" 'cfw:open-org-calendar)
(define-key leader-map "ol" 'org-agenda-list)
(define-key leader-map "ob" 'org-iswitchb)
(define-key leader-map "c" 'org-capture)
(define-key leader-map "oe" 'edit-todo)
(define-key leader-map "ol" 'org-store-link)
(define-key leader-map "pb" 'counsel-projectile-switch-to-buffer)
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
(define-key leader-map "ww" 'other-window)
(define-key leader-map [? ] 'counsel-M-x)
(define-key leader-map [C-p] 'projectile-switch-project)
(define-key leader-map [C-v] 'hrs/search-project-for-symbol-at-point)
(define-key leader-map [tab] 'mode-line-other-buffer)
(define-key leader-map [esc] 'evil-ex-nohighlight)

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

;; Treat _ as part of a word-object
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; Highlight current line
(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))

;; Line numbers
(use-package linum-relative
  :ensure t
  :after evil
  :custom-face (linum ((nil (:background nil))))
  :config
  (global-linum-mode)
  (linum-relative-global-mode)
  (setq linum-format " %d "
        linum-relative-current-symbol ""))

;; Git Gutter
(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode +1)
  (git-gutter:linum-setup))

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
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

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
  ;; :diminish
  :hook (prog-mode-hook flycheck-mode)
  :config

  ;; ;; Flycheck
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-mode)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (use-package flycheck-pos-tip
    :hook (flycheck-mode-hook flycheck-pos-tip-mode))
  (use-package flycheck-color-mode-line
    :hook (flycheck-mode-hook flycheck-color-mode-line-mode)))

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

(use-package ag)

;; Ivy
;; ===
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  (add-to-list 'ivy-ignore-buffers "\\*magit"))

;; Swiper
;; ===

(use-package swiper
  :bind
  ("C-s" . swiper))

;; Dired
;; ===

(require 'dired)
(use-package ranger
  :config
  (ranger-override-dired-mode t))

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

;; Python
;; ===

(setq-default python-indent 4)
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

;; Anaconda mode
(use-package anaconda-mode
  :hook ((python-mode-hook anaconda-mode)
         (python-mode-hook anaconda-eldoc-mode)))

;; Autocompletion
(use-package company-anaconda
  :after anaconda-mode
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda))))

;; Disable Aggressive Indentation
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

;; Highlight Indent Guides
(use-package highlight-indent-guides
  :hook (python-mode-hook . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; Use yapf to format
(use-package yapfify
  :hook (python-mode-hook . yapf-mode))

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

;; Preprocessors
(use-package pug-mode)

;; CSS
;; ===

(setq-default css-indent-offset 2)
(use-package less-css-mode
  :hook less-mode-hook)
(use-package sass-mode
  :hook sass-mode-hook)
(use-package rainbow-mode
  :hook ((css-mode-hook . rainbow-mode)
         (sass-mode-hook . rainbow-mode)
         (html-mode-hook . rainbow-mode)
         (less-mode-hook . rainbow-mode))
  :diminish rainbow-mode)

;; JavaScript
;; ===

(setq-default js-indent-level 2)

;; Prettier
(use-package prettier-js
  :hook ((js2-mode-hook . prettier-js-mode)
         (web-mode-hook . prettier-js-mode)
         (vue-mode-hook . prettier-js-mode)
         (rjsx-mode-hook . prettier-js-mode))
  :ensure nil
  :quelpa (prettier-js
           :fetcher github
           :repo "prettier/prettier-emacs"
           :branch "prettier-js-prettify-region")
  :config
  (defun prettier-vue ()
    (interactive)
    (progn
      (let ((original (point)))
        (goto-char 0)
        (let* ((script-start (re-search-forward "<script>" nil t))
               (start (+ script-start 1))
               (script-end (re-search-forward "</script>" nil t))
               (end (- script-end 9)))
          (prettier-js--prettify start end)
          (goto-char original)))
      (let ((original (point)))
        (goto-char 0)
        (let* ((script-start (re-search-forward "<template>" nil t))
               (start (+ script-start 1))
               (script-end (re-search-forward "</template>" nil t))
               (end (- script-end 11)))
          ;; (sgml-pretty-print start end)
          (indent-region start end)
          (goto-char original)))
      (vue-mode-reparse)))
  (add-hook 'vue-mode-hook
            (lambda ()
              (prettier-js-mode)
              (add-hook 'before-save-hook 'prettier-vue nil 'local)))
  (setq prettier-js-args '("--trailing-comma" "es5"
                           "--no-semi"
                           "--single-quote")))

;; React
(use-package rjsx-mode
  :mode ("\\.js\\'" . rjsx-mode)
  :config
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors nil
        js2-indent-level 2
        js2-basic-offset 2
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil))

;; Vue
(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode)
  :config
  (setq mmm-submode-decoration-level 0))

;; Org
;; ===

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure org-plus-contrib
  :hook (org-mode-hook flyspell-mode)
  :config
  (setq org-agenda-files (quote ("~/Notes/todo.org" "~/Notes/inbox.org" "~/Notes/calendar.org")))
  
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/Notes/inbox.org")
                 "* TODO %?"))))

  (set-face-attribute 'org-level-1 nil :weight 'bold :height 1.20)
  (set-face-attribute 'org-level-2 nil :weight 'bold :height 1.15)
  (set-face-attribute 'org-level-3 nil :weight 'bold :height 1.10)
  (set-face-attribute 'org-level-4 nil :weight 'bold :height 1.05)
  (set-face-attribute 'org-level-5 nil :weight 'bold :height 1.00)

  (setq org-ellipsis " ▼"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-html-postamble nil
        org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-startup-with-latex-preview t
        org-format-latex-options (plist-put org-format-latex-options :scale 0.8)
        org-startup-with-inline-images t)

  (define-key org-mode-map (kbd "$") (lambda ()
                                       (interactive)
                                       (insert "$")
                                       (left-char 1)
                                       (org-toggle-latex-fragment)
                                       (right-char 1))))

;; Calendar
(use-package calfw)
(use-package calfw-org
  :after calfw)

(use-package org-gcal
  :config
  (setq org-gcal-client-id "79887467989-4inb8t4kbrmqmb7453edv7i9bagsr673.apps.googleusercontent.com"
        org-gcal-file-alist '(("goweiwen@gmail.com" . "~/Notes/calendar.org"))))


;; Coq
;; ===
(use-package company-coq
  :mode ("\\.v\\'" . coq-mode)
  :hook (('coq-mode-hook . 'company-coq-mode)
         ('coq-mode-hook . 'coq-symbols-list))
  :bind (:map company-coq-map
              ("M-j" . proof-assert-next-command-interactive)
              ("M-k" . proof-undo-last-successful-command)
              ("M-<return>" . proof-goto-point))
  :custom-face
  (proof-locked-face ((nil (:background "#003300"))))
  (proof-queue-face ((nil (:background "#116611"))))
  :config

  ;; Appearance

  ;; Use Coq 8.6 (for CS3234)
  (setenv "PATH" (concat (getenv "PATH") "/Users/weiwen/.opam/4.05.0/bin"))
  (setq exec-path (append exec-path '("/Users/weiwen/.opam/4.05.0/bin")))

  ;; Prettify symbols
  (company-coq-features/prettify-symbols nil)
  (setq coq-symbols-list
        '(lambda()
           (setq prettify-symbols-alist
                 (quote
                  (("|-" . 8866)
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
                   ("<~~" . 11059)))))))

;; Export
(require 'ox-md)
(require 'ox-beamer)

;; Other Languages
;; ===

(use-package coffee-mode
  :mode "\\.coffee\\'")
(use-package lua-mode
  :mode "\\.lua\\'")
(use-package elm-mode
  :mode "\\.elm\\'")
(use-package json-mode
  :mode "\\.json\\'")
(use-package markdown-mode
  :mode "\\.md\\'")
(use-package haskell-mode
  :mode "\\.hs\\'")
(use-package swift-mode
  :mode "\\.swift\\'")

;; Writing
;; ===

;; ;; Darkroom
;; (use-package darkroom
;;   :config
;;   (setq darkroom-text-scale-increase 0)
;;   (add-hook 'org-mode-hook 'darkroom-tentative-mode))

;; Use variable pitch for text modes
(use-package mixed-pitch
  :hook ('org-mode-hook 'mixed-pitch-mode))

;; ;; Proselint
;; (require 'flycheck)
;; (flycheck-define-checker proselint
;;   "A linter for prose."
;;   :command ("proselint" source-inplace)
;;   :error-patterns
;;   ((warning line-start (file-name) ":" line ":" column ": "
;;             (id (one-or-more (not (any " "))))
;;             (message (one-or-more not-newline)
;;                      (zero-or-more "\n" (any " ") (one-or-more not-newline)))
;;             line-end))
;;   :modes (text-mode markdown-mode gfm-mode org-mode))
;; (add-to-list 'flycheck-checkers 'proselint)

;; (add-hook 'text-mode-hook #'flycheck-mode)

;; Automatically generated

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (frames-only-mode mixed-pitch calfw-org calfw mode-icons nlinum-relative evil-visual-mark-mode company-coq markdown-mode elm-mode vue-mode wc-mode ranger olivetti olivetti-mode writeroom-mode minimap evil-snipe evil-mc company-anaconda py-yapf ag))))

;;; init.el ends here
