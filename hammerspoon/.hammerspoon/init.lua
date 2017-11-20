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
bind('ctrl-cmd-alt', 'r', function()
  hs.execute('~/.chunkwmrc')
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
bind(one, 'return',
     function() hs.applescript('tell application "iTerm" to create window with default profile') end)

-- Finder
bind(one, 'e', execute('open ~'))

-- Do not disturb
bind(one, 'n', compose2(
       function() hs.alert(hs.caffeinate.toggle('displayIdle') and 'Caffeine' or 'Decaff') end,
       keyStroke('ctrl-cmd', 'n')))

--
-- chunkwm
--

-- Focus window
bind(one, 'h', execute('chunkc tiling:window --focus west'))
bind(one, 'j', execute('chunkc tiling:window --focus south'))
bind(one, 'k', execute('chunkc tiling:window --focus north'))
bind(one, 'l', execute('chunkc tiling:window --focus east'))

-- Move window
bind(two, 'h', execute('chunkc tiling:window --warp west'))
bind(two, 'j', execute('chunkc tiling:window --warp south'))
bind(two, 'k', execute('chunkc tiling:window --warp north'))
bind(two, 'l', execute('chunkc tiling:window --warp east'))

-- Trackpad Gestures
-- bind(one, 'up', execute('chunkc tiling:monitor -f prev'))
-- bind(one, 'down', execute('chunkc tiling:monitor -f next'))
bind(one, 'up', nil, keyStroke('ctrl', 'up'))
bind(one, 'down', nil, keyStroke('ctrl', 'down'))
bind(one, 'left', nil, keyStroke('ctrl', 'right'))
bind(one, 'right', nil, keyStroke('ctrl', 'left'))

-- Move Space/Monitor
bind('ctrl-shift', 'up', execute('chunkc tiling:window --send-to-monitor next'))
bind('ctrl-shift', 'down', execute('chunkc tiling:window --send-to-monitor prev'))
bind('ctrl-shift', 'left', compose2(
       execute('chunkc tiling:window --send-to-desktop prev'),
       keyStroke('ctrl', 'left')))
bind('ctrl-shift', 'right', compose2(
       execute('chunkc tiling:window --send-to-desktop next'),
       keyStroke('ctrl', 'right')))

for i = 1, 9 do
  bind('ctrl-shift', tostring(i), compose2(
         execute('chunkc tiling:window --send-to-desktop ' .. tostring(i)),
         keyStroke('ctrl', tostring(i))))
end

-- Resize mode
local main = hs.hotkey.modal.new(one, ',')
function main:entered()
  self.alert = hs.alert([[
  x - Mirror Vertically
  y - Mirror Horizontally
  r - Rotate
  hjkl - Increase Size
  HJKL - Decrease Size
  b - Toggle Borders
  m - Maximise
  f - Fullscreen
  z - Zoom to Parent
  e - Toggle Split
  t - Toggle Float
  s - Toggle PIP]], 10)
end
function main:exited()
  if self.alert ~= nil then
    hs.alert.closeSpecific(self.alert)
    self.alert = nil
  end
end
main:bind('', 'x', execute('chunkc tiling:desktop --mirror vertical'))
main:bind('', 'y', execute('chunkc tiling:desktop --mirror horizontal'))
main:bind('', 'r', execute('chunkc tiling:desktop --rotate 90'))
main:bind('', 'h', execute('chunkc tiling:window --use-temporary-ratio 0.1 --adjust-window-edge west'))
main:bind('', 'j', execute('chunkc tiling:window --use-temporary-ratio 0.1 --adjust-window-edge south'))
main:bind('', 'k', execute('chunkc tiling:window --use-temporary-ratio 0.1 --adjust-window-edge north'))
main:bind('', 'l', execute('chunkc tiling:window --use-temporary-ratio 0.1 --adjust-window-edge east'))
main:bind('shift', 'h', execute('chunkc tiling:window --use-temporary-ratio -0.1 --adjust-window-edge east'))
main:bind('shift', 'j', execute('chunkc tiling:window --use-temporary-ratio -0.1 --adjust-window-edge north'))
main:bind('shift', 'k', execute('chunkc tiling:window --use-temporary-ratio -0.1 --adjust-window-edge south'))
main:bind('shift', 'l', execute('chunkc tiling:window --use-temporary-ratio -0.1 --adjust-window-edge west'))
main:bind('', 'escape', function()
            hs.alert('Esc')
            main:exit()
end)
main:bind('', 'b', exitAfter(main, execute('chunkc tiling:desktop --toggle offset')))
main:bind('', 'm', exitAfter(main, execute('chunkc tiling:window --toggle fullscreen')))
main:bind('', 'f', exitAfter(main, execute('chunkc tiling:window --toggle native-fullscreen')))
main:bind('', 'z', exitAfter(main, execute('chunkc tiling:window --toggle parent')))
main:bind('', 'e', exitAfter(main, execute('chunkc tiling:window --toggle split')))
main:bind('', 't', exitAfter(main, execute('chunkc tiling:window --toggle float')))
main:bind('', 's', exitAfter(main, execute('chunkc tiling::window --toggle sticky; chunkc tiling::window --warp-floating pip-right')))

-- Next layout
local layouts = {'bsp', 'monocle', 'float'}
local layout = 1
main:bind('', 'space', function()
       layout = layout % #layouts + 1
       hs.execute('chunkc tiling:desktop --layout ' .. layouts[layout], true)
       hs.alert(layouts[layout]:gsub('^%l', string.upper))
                   end, exit)

-- -- Window
-- bind(one, 'b', execute('chunkc tiling:desktop --toggle offset'))
-- bind(one, 'm', execute('chunkc tiling:window --toggle fullscreen'))
-- bind(one, 'f', execute('chunkc tiling:window --toggle native-fullscreen'))
-- bind(one, 'z', execute('chunkc tiling:window --toggle parent'))
-- bind(one, 'e', execute('chunkc tiling:window --toggle split'))
-- bind(one, 't', execute('chunkc tiling:window --toggle float'))
-- bind(one, 's', execute('chunkc tiling::window --toggle sticky; chunkc tiling::window --warp-floating pip-right'))

-- Vim bindings
local vim = hs.hotkey.modal.new(one, 'escape')
vim:bind('', 'escape', function() vim:exit() end)
  :bind('', 'd', keyStroke('', 'space'), nil, keyStroke('', 'space'))
  :bind('', 'e', keyStroke('shift', 'space'), nil, keyStroke('shift', 'space'))
  :bind('', 'h', keyStroke('', 'left'), nil, keyStroke('', 'left'))
  :bind('', 'j', keyStroke('', 'down'), nil, keyStroke('', 'down'))
  :bind('', 'k', keyStroke('', 'up'), nil, keyStroke('', 'up'))
  :bind('', 'l', keyStroke('', 'right'), nil, keyStroke('', 'right'))

-- Preview.app
local wf = hs.window.filter
wf.new {'Preview', 'Finder'}
  :subscribe(wf.windowFocused, function() vim:enter() end)
  :subscribe(wf.windowUnfocused, function() vim:exit() end)

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
