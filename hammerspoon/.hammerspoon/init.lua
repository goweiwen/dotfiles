local spaces = require 'hs._asm.undocumented.spaces'

local one = 'ctrl-cmd'
local two = 'ctrl-shift-cmd'
local three = 'ctrl-alt'
local four = 'ctrl-alt-shift'

-- Util
local bind = hs.hotkey.bind
local new = hs.hotkey.new

function execute(cmd)
  return function() hs.execute(cmd, true) end
end

local delay = hs.eventtap.keyRepeatInterval()
function keyStroke(mod, key)
  return function() hs.timer.doAfter(0.01, function() hs.eventtap.keyStroke(mod, key, delay) end) end
end

function compose2(a, b)
  return function() a() b() end
end

function exitAfter(mode, fn)
  return function() fn() mode:exit() end
end

-- Reload
bind('ctrl-cmd-alt', 'r', function() hs.reload() end)

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
bind(one, 'return',
     function() hs.applescript('tell application "iTerm" to create window with default profile') end)

-- Finder
bind(one, 'e', execute('open ~'))

-- Do not disturb
bind(one, 'n', compose2(
       function() hs.alert(hs.caffeinate.toggle('displayIdle') and 'Caffeine' or 'Decaff') end,
       keyStroke('ctrl-cmd', 'n')))

--
-- Tiling
--

hs.grid.MARGINX = 0
hs.grid.MARGINY = 0
hs.grid.GRIDWIDTH = 6
hs.grid.GRIDHEIGHT = 2

-- Snap windows
bind(one, ';', function() hs.grid.snap(hs.window.focusedWindow()) end)
bind(one, "'", function() hs.fnutil.map(hs.window.visibleWindows(), hs.grid.snap) end)

-- Window Hints
bind(one, '.', function() hs.hints.windowHints(hs.window.allWindows()) end)
bind(one, '/', hs.grid.toggleShow)

-- Hotkeys
bind(one, 'm', hs.grid.maximizeWindow)
bind(one, 'b', function() hs.window.focusedWindow():centerOnScreen():setFrameInScreenBounds() end)
bind(one, 'c', function() hs.dialog.color.show() end)

-- Focus window
bind(one, 'h', function() hs.window.focusedWindow():focusWindowWest() end)
bind(one, 'l', function() hs.window.focusedWindow():focusWindowEast() end)
bind(one, 'k', function() hs.window.focusedWindow():focusWindowNorth() end)
bind(one, 'j', function() hs.window.focusedWindow():focusWindowSouth() end)

-- Move window
bind(two, 'h', hs.grid.pushWindowLeft)
bind(two, 'l', hs.grid.pushWindowRight)
bind(two, 'k', hs.grid.pushWindowUp)
bind(two, 'j', hs.grid.pushWindowDown)
bind(two, 'E', hs.grid.pushWindowNextScreen)
bind(two, 'R', hs.grid.pushWindowPrevScreen)

-- Move window to spaces (hacky)
function moveWindowNextSpace() moveWindowRelativeSpace(1) end
function moveWindowPrevSpace() moveWindowRelativeSpace(-1) end

function moveWindowRelativeSpace(n)
  local allSpaces = spaces.query()
  local activeSpace = spaces.activeSpace()
  for i, v in ipairs(allSpaces) do
    if v == activeSpace then
      return hs.window.focusedWindow():spacesMoveTo(allSpaces[(i - n - 1) % #allSpaces + 1])
    end
  end
end

function moveWindowToSpace(n)
  local allSpaces = spaces.query()
  local activeSpace = spaces.activeSpace()
  return hs.window.focusedWindow():spacesMoveTo(allSpaces[#allSpaces - n + 1])
end

bind('ctrl-shift', 'left', nil, compose2(moveWindowPrevSpace, keyStroke('ctrl', 'left')))
bind('ctrl-shift', 'right', nil, compose2(moveWindowNextSpace, keyStroke('ctrl', 'right')))
for i = 1, 9 do
  bind('ctrl-shift', tostring(i), nil, compose2(
    function() moveWindowToSpace(i) end,
    keyStroke('ctrl', tostring(i))
  ))
end

-- Resize window
local resizeMode = hs.hotkey.modal.new(one, 'r', 'Resize')
resizeMode:bind('', 'h', hs.grid.resizeWindowThinner)
resizeMode:bind('', 'j', hs.grid.resizeWindowTaller)
resizeMode:bind('', 'k', hs.grid.resizeWindowShorter)
resizeMode:bind('', 'l', hs.grid.resizeWindowWider)
resizeMode:bind('', 'Escape', function() resizeMode:exit() end)
resizeMode:bind('', 'Return', function() resizeMode:exit() end)

-- Mouse follows Focus
local allWindowsFilter = hs.window.filter.new()
allWindowsFilter:subscribe(hs.window.filter.windowFocused, function(window)
  local size = window:size()
  local position = window:topLeft()
  local center = {
    x = position.x + size.w / 2,
    y = position.y + size.h / 2,
  }
  hs.mouse.setAbsolutePosition(center)
end)

-- Trackpad Gestures
-- bind(one, 'up', execute('chunkc tiling:monitor -f prev'))
-- bind(one, 'down', execute('chunkc tiling:monitor -f next'))
bind(one, 'up', nil, keyStroke('ctrl', 'up'))
bind(one, 'down', nil, keyStroke('ctrl', 'down'))
bind(one, 'left', nil, keyStroke('ctrl', 'right'))
bind(one, 'right', nil, keyStroke('ctrl', 'left'))

-- Vim bindings
local vim = hs.hotkey.modal.new(one, 'i')
vim:bind('', 'escape', function() vim:exit() end)
  :bind('', 'd', keyStroke('', 'space'), nil, keyStroke('', 'space'))
  :bind('', 'e', keyStroke('shift', 'space'), nil, keyStroke('shift', 'space'))
  :bind('', 'h', keyStroke('', 'left'), nil, keyStroke('', 'left'))
  :bind('', 'j', keyStroke('', 'down'), nil, keyStroke('', 'down'))
  :bind('', 'k', keyStroke('', 'up'), nil, keyStroke('', 'up'))
  :bind('', 'l', keyStroke('', 'right'), nil, keyStroke('', 'right'))

hs.alert('Config loaded.')
