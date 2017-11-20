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
function pushWindowNextSpace()
  local allSpaces = spaces.query()
  local activeSpace = spaces.activeSpace()
  for i, v in ipairs(allSpaces) do
    if v == activeSpace then
      return hs.window.focusedWindow():spacesMoveTo(allSpaces[(i - 2) % #allSpaces + 1])
    end
  end
end

function pushWindowPrevSpace()
  local allSpaces = spaces.query()
  local activeSpace = spaces.activeSpace()
  for i, v in ipairs(allSpaces) do
    if v == activeSpace then
      return hs.window.focusedWindow():spacesMoveTo(allSpaces[i % #allSpaces + 1])
    end
  end
end

function pushWindowToSpace(n)
  local allSpaces = spaces.query()
  local activeSpace = spaces.activeSpace()
  return hs.window.focusedWindow():spacesMoveTo(allSpaces[#allSpaces - n + 1])
end

bind('ctrl-shift', 'left', nil, compose2(pushWindowPrevSpace, keyStroke('ctrl', 'left')))
bind('ctrl-shift', 'right', nil, compose2(pushWindowNextSpace, keyStroke('ctrl', 'right')))
for i = 1, 9 do
  bind('ctrl-shift', tostring(i), nil, compose2(
    function() pushWindowToSpace(i) end,
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

-- Trackpad Gestures
-- bind(one, 'up', execute('chunkc tiling:monitor -f prev'))
-- bind(one, 'down', execute('chunkc tiling:monitor -f next'))
bind(one, 'up', nil, keyStroke('ctrl', 'up'))
bind(one, 'down', nil, keyStroke('ctrl', 'down'))
bind(one, 'left', nil, keyStroke('ctrl', 'right'))
bind(one, 'right', nil, keyStroke('ctrl', 'left'))

-- Vim bindings
local vim = hs.hotkey.modal.new(one, 'escape')
vim:bind('', 'escape', function() vim:exit() end)
  :bind('', 'd', keyStroke('', 'space'), nil, keyStroke('', 'space'))
  :bind('', 'e', keyStroke('shift', 'space'), nil, keyStroke('shift', 'space'))
  :bind('', 'h', keyStroke('', 'left'), nil, keyStroke('', 'left'))
  :bind('', 'j', keyStroke('', 'down'), nil, keyStroke('', 'down'))
  :bind('', 'k', keyStroke('', 'up'), nil, keyStroke('', 'up'))
  :bind('', 'l', keyStroke('', 'right'), nil, keyStroke('', 'right'))

-- Mouse
function mouseMove(x, y)
  return function()
    local point = hs.mouse.getAbsolutePosition()
    hs.mouse.setAbsolutePosition({ x = point.x + x, y = point.y + y })
  end
end

function scrollWheel(x, y)
  return function()
    hs.eventtap.scrollWheel({ x, y }, {})
  end
end

local mouse = hs.hotkey.modal.new(one, '\\')
mouse:bind('', 'escape', function() mouse:exit() end)
  :bind('', 'h', mouseMove(-10, 0), nil, mouseMove(-10, 0))
  :bind('', 'j', mouseMove(0, 10), nil, mouseMove(0, 10))
  :bind('', 'k', mouseMove(0, -10), nil, mouseMove(0, -10))
  :bind('', 'l', mouseMove(10, 0), nil, mouseMove(10, 0))
  :bind('shift', 'h', mouseMove(-50, 0), nil, mouseMove(-50, 0))
  :bind('shift', 'j', mouseMove(0, 50), nil, mouseMove(0, 50))
  :bind('shift', 'k', mouseMove(0, -50), nil, mouseMove(0, -50))
  :bind('shift', 'l', mouseMove(50, 0), nil, mouseMove(50, 0))
  :bind('', 'space', function() hs.eventtap.leftClick(hs.mouse.getAbsolutePosition()) end)
  :bind('shift', 'space', function() hs.eventtap.rightClick(hs.mouse.getAbsolutePosition()) end)
  :bind('', 'e', scrollWheel(0, 3), nil, scrollWheel(0, 3))
  :bind('', 'd', scrollWheel(0, -3), nil, scrollWheel(0, -3))
  :bind('shift', 'e', scrollWheel(0, 15), nil, scrollWheel(0, 15))
  :bind('shift', 'd', scrollWheel(0, -15), nil, scrollWheel(0, -15))

hs.alert('Config loaded.')
