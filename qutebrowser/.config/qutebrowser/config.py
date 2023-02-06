import catppuccin

config.load_autoconfig()

catppuccin.setup(c, 'mocha')

config.bind('<Ctrl-l>', 'spawn --userscript qute-bitwarden --totp')
