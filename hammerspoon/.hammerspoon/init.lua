require 'utils'

-- Reload
bind('ctrl-cmd-alt', 'r', function()
    hs.alert('Reloading config...')
    hs.reload()
  end)

--
-- Style
--

hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.75 }
hs.alert.defaultStyle.strokeColor = { white = 0, alpha = 0 }
hs.alert.defaultStyle.strokeWidth = 0
hs.alert.defaultStyle.radius = 32

--
-- Launchers
--

-- Terminal
bind(one, 'return', function() hs.applescript('tell application "iTerm" to create window with default profile') end)
-- bind(one, 'return', execute('open -nb io.alacritty'))

-- Do not disturb
bind(one, 'n', compose(
       function() hs.alert(hs.caffeinate.toggle('displayIdle') and 'Caffeine' or 'Decaff') end,
       keyStroke('ctrl-cmd', 'n')))

require 'bindings/runner'
-- require 'bindings/hackintosh'
-- require 'bindings/tiling'
require 'bindings/chunkwm'
require 'bindings/vim'

hs.alert('Config loaded.')
