spaces = require 'hs._asm.undocumented.spaces'

local zeroth = 'ctrl'
local first = 'ctrl-cmd'
local second = 'ctrl-shift-cmd'

hs.grid.MARGINX   = 10
hs.grid.MARGINY   = 10
hs.grid.GRIDWIDTH   = 3
hs.grid.GRIDHEIGHT   = 2

-- global operations
hs.hotkey.bind(first, ';', function() hs.grid.snap(hs.window.focusedWindow()) end)
hs.hotkey.bind(first, "'", function() hs.fnutils.map(hs.window.visibleWindows(), hs.grid.snap) end)

-- adjust grid size
hs.hotkey.bind(first, '=', function() hs.grid.adjustWidth( 1) end)
hs.hotkey.bind(first, '-', function() hs.grid.adjustWidth(-1) end)
hs.hotkey.bind(first, ']', function() hs.grid.adjustHeight( 1) end)
hs.hotkey.bind(first, '[', function() hs.grid.adjustHeight(-1) end)

-- Window Hints
hs.hotkey.bind(first, '.', function() hs.hints.windowHints(hs.window.allWindows()) end)
hs.hotkey.bind(first, '/', hs.grid.toggleShow)

-- change focus
hs.hotkey.bind(first, 'H', function() hs.window.focusedWindow():focusWindowWest() end)
hs.hotkey.bind(first, 'J', function() hs.window.focusedWindow():focusWindowSouth() end)
hs.hotkey.bind(first, 'K', function() hs.window.focusedWindow():focusWindowNorth() end)
hs.hotkey.bind(first, 'L', function() hs.window.focusedWindow():focusWindowEast() end)
hs.hotkey.bind(first, 'left', function() hs.window.focusedWindow():focusWindowWest() end)
hs.hotkey.bind(first, 'down', function() hs.window.focusedWindow():focusWindowSouth() end)
hs.hotkey.bind(first, 'up', function() hs.window.focusedWindow():focusWindowNorth() end)
hs.hotkey.bind(first, 'right', function() hs.window.focusedWindow():focusWindowEast() end)

-- multi monitor
hs.hotkey.bind(first, 'E', hs.grid.pushWindowNextScreen)
hs.hotkey.bind(first, 'R', hs.grid.pushWindowPrevScreen)

-- move windows
hs.hotkey.bind(second, 'H', hs.grid.pushWindowLeft)
hs.hotkey.bind(second, 'J', hs.grid.pushWindowDown)
hs.hotkey.bind(second, 'K', hs.grid.pushWindowUp)
hs.hotkey.bind(second, 'L', hs.grid.pushWindowRight)
hs.hotkey.bind(second, 'left', hs.grid.pushWindowLeft)
hs.hotkey.bind(second, 'down', hs.grid.pushWindowDown)
hs.hotkey.bind(second, 'up', hs.grid.pushWindowUp)
hs.hotkey.bind(second, 'right', hs.grid.pushWindowRight)
hs.hotkey.bind(first, 'M', hs.grid.maximizeWindow)
hs.hotkey.bind(first, 'M', hs.grid.maximizeWindow)

-- resize windows
local resizeMode = hs.hotkey.modal.new(first, 'R', 'Resize')
resizeMode:bind('', 'H', hs.grid.resizeWindowThinner)
resizeMode:bind('', 'J', hs.grid.resizeWindowShorter)
resizeMode:bind('', 'K', hs.grid.resizeWindowTaller)
resizeMode:bind('', 'L', hs.grid.resizeWindowWider)
resizeMode:bind('', 'left', hs.grid.resizeWindowThinner)
resizeMode:bind('', 'down', hs.grid.resizeWindowShorter)
resizeMode:bind('', 'up', hs.grid.resizeWindowTaller)
resizeMode:bind('', 'right', hs.grid.resizeWindowWider)
resizeMode:bind('', 'Escape', function() resizeMode:exit() end)
resizeMode:bind('', 'Return', function() resizeMode:exit() end)

function nextSpace()
  allSpaces = spaces.query()
  activeSpace = spaces.activeSpace()
  for i, v in ipairs(allSpaces) do
    if v ==  activeSpace then
      return allSpaces[(i-2) % #allSpaces + 1]
    end
  end
end

function prevSpace()
  allSpaces = spaces.query()
  activeSpace = spaces.activeSpace()
  for i, v in ipairs(allSpaces) do
    if v ==  activeSpace then
      return allSpaces[(i) % #allSpaces + 1]
    end
  end
end

function activeSpace()
  local allSpaces = spaces.query()
  local activeSpace = spaces.activeSpace()
  for i, v in ipairs(allSpaces) do
    if v ==  activeSpace then
      return i
    end
  end
  return 1
end

-- move spaces
hs.hotkey.bind('ctrl-cmd', 'right', nil, function() hs.timer.doAfter(0.01, function() hs.eventtap.keyStroke('ctrl', 'left') end) end)
hs.hotkey.bind('ctrl-cmd', 'left', nil, function() hs.timer.doAfter(0.01, function() hs.eventtap.keyStroke('ctrl', 'right') end) end)
hs.hotkey.bind('ctrl-shift', 'left', nil, function() hs.timer.doAfter(0.01, function() hs.window.focusedWindow():spacesMoveTo(prevSpace()); hs.eventtap.keyStroke('ctrl', 'left') end) end)
hs.hotkey.bind('ctrl-shift', 'right', nil, function() hs.timer.doAfter(0.01, function() hs.window.focusedWindow():spacesMoveTo(nextSpace()); hs.eventtap.keyStroke('ctrl', 'right') end) end)
-- hs.hotkey.bind('ctrl-shift', 'left', function() hs.window.focusedWindow():spacesMoveTo(prevSpace()) end, function() spaces.changeToSpace(prevSpace()) end)
-- hs.hotkey.bind('ctrl-shift', 'right', function() hs.window.focusedWindow():spacesMoveTo(nextSpace()) end, function() spaces.changeToSpace(nextSpace()) end)

-- mission control/expose
hs.hotkey.bind('ctrl-cmd', 'up', nil, function() hs.timer.doAfter(0.01, function() hs.eventtap.keyStroke('ctrl', 'up') end) end)
hs.hotkey.bind('ctrl-cmd', 'down', nil, function() hs.timer.doAfter(0.01, function() hs.eventtap.keyStroke('ctrl', 'down') end) end)

-- Reload
hs.hotkey.bind('ctrl-shift-cmd', 'R', function() hs.reload() end)

hs.alert('Config loaded.')
-- hs.notify.new({ title = "Hammerspoon", informativeText = "Config loaded." }):send()
