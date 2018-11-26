(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :config
  (setq lua-indent-level 2)
  (setq lua-documentation-url "http://www.lua.org/manual/5.3/manual.html#pdf-")
  (setq lua-documentation-function 'eww-browse-url))

(provide 'lang/lua)
