import catppuccin

config.load_autoconfig()

catppuccin.setup(c, 'mocha')

config.bind('<Ctrl-Shift-l>', 'spawn --userscript qute-bitwarden --totp')

config.bind('<Ctrl-j>', 'config-cycle --print content.javascript.enabled false true')

config.bind('d', 'scroll-page 0 0.5')
config.bind('e', 'scroll-page 0 -0.5')
config.bind('x', 'tab-close')
config.bind('X', 'tab-close -o')

c.fonts.default_family = "Terminus"
c.fonts.default_size = "9pt"

c.tabs.title.format = "{current_title}"
c.window.title_format = "{current_title}"

c.url.default_page = "about:blank"
c.url.start_pages = "about:blank"

c.content.prefers_reduced_motion = True
