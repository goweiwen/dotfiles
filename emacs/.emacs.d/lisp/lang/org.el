(use-package org
  :mode ("\\.org$" . org-mode)
  :ensure org-plus-contrib
  :hook (org-mode-hook flyspell-mode)
  :config
  (require 'ox-md)
  (require 'ox-beamer)
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

(provide 'lang/org)
