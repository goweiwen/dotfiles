(setq-default js-indent-level 2)

;; Prettier
(use-package prettier-js
  :mode "\\.jsx?$"
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

(use-package rjsx-mode
  :mode ("\\.jsx?$" . rjsx-mode)
  :config
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors nil
        js2-indent-level 2
        js2-basic-offset 2
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil))

(provide 'lang/js)
