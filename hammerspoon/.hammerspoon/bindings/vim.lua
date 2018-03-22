require 'utils'

-- Vim bindings
local vim = hs.hotkey.modal.new(one, 'i')
vim:bind('', 'escape', function() vim:exit() end)
  :bind('', 'd', keyStroke('', 'space'), nil, keyStroke('', 'space'))
  :bind('', 'e', keyStroke('shift', 'space'), nil, keyStroke('shift', 'space'))
  :bind('', 'h', keyStroke('', 'left'), nil, keyStroke('', 'left'))
  :bind('', 'j', keyStroke('', 'down'), nil, keyStroke('', 'down'))
  :bind('', 'k', keyStroke('', 'up'), nil, keyStroke('', 'up'))
  :bind('', 'l', keyStroke('', 'right'), nil, keyStroke('', 'right'))
