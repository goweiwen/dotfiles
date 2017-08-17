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

-- Reload
bind('ctrl-cmd-alt', 'r', function() hs.reload() end)

--
-- Launchers
--

-- Terminal
bind('cmd', 'return',
     function() hs.applescript('tell application "iTerm" to create window with default profile') end)

-- Finder
bind('cmd', 'e', execute('open ~'))

-- Do not disturb
bind(one, 'n', compose2(
       function() hs.alert(hs.caffeinate.toggle('displayIdle') and 'Caffeine' or 'Decaff') end,
       keyStroke('ctrl-cmd', 'n')))

-- Trackpad Gestures
bind(one, 'up', nil, keyStroke('ctrl', 'up'))
bind(one, 'down', nil, keyStroke('ctrl', 'down'))
bind(one, 'left', nil, keyStroke('ctrl', 'left'))
bind(one, 'right', nil, keyStroke('ctrl', 'right'))

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

-- Focus Space/Monitor
bind(three, 'h', nil, keyStroke('ctrl', 'left'))
bind(three, 'j', execute('chunkc tiling:monitor --focus prev'))
bind(three, 'k', execute('chunkc tiling:monitor --focus next'))
bind(three, 'l', nil, keyStroke('ctrl', 'right'))

-- Move Space/Monitor
bind(four, 'h', compose2(
       execute('chunkc tiling:window --send-to-desktop prev'),
       keyStroke('ctrl', 'left')))
bind(four, 'j', execute('chunkc tiling:window --send-to-monitor prev'))
bind(four, 'k', execute('chunkc tiling:window --send-to-monitor next'))
bind(four, 'l', compose2(
       execute('chunkc tiling:window --send-to-desktop next'),
       keyStroke('ctrl', 'right')))

bind('ctrl-shift', 'left', compose2(
       execute('chunkc tiling:window --send-to-desktop prev'),
       keyStroke('ctrl', 'left')))
bind('ctrl-shift', 'right', compose2(
       execute('chunkc tiling:window --send-to-desktop next'),
       keyStroke('ctrl', 'right')))

for i = 1, 4 do
  bind(four, tostring(i), compose2(
         execute('chunkc tiling:window --send-to-desktop ' .. tostring(i)),
         keyStroke('ctrl', tostring(i))))
end

-- Resize mode
local main = hs.hotkey.modal.new(one, ',', 'Window')
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

-- Window
bind(one, 'b', execute('chunkc tiling:desktop --toggle offset'))
bind(one, 'm', execute('chunkc tiling:window --toggle fullscreen'))
bind(one, 'f', execute('chunkc tiling:window --toggle native-fullscreen'))
bind(one, 'z', execute('chunkc tiling:window --toggle parent'))
bind(one, 't', execute('chunkc tiling:window --toggle float'))

-- Next layout
local layouts = {'bsp', 'monocle', 'float'}
local layout = 1
bind(one, 'space', function()
       layout = layout % #layouts + 1
       hs.execute('chunkc tiling:desktop --layout ' .. layouts[layout], true)
       hs.alert(layouts[layout]:gsub('^%l', string.upper))
                   end, exit)

-- Vim bindings
vim = {}
function vim.bind() for i, v in ipairs(vim.binds) do v:enable() end end
  function vim.unbind() for i, v in ipairs(vim.binds) do v:disable() end end

    vim.binds = {
      new('', 'd', keyStroke('', 'space'), nil, keyStroke('', 'space')),
      new('', 'e', keyStroke('shift', 'space'), nil, keyStroke('shift', 'space')),
      new('', 'h', keyStroke('', 'left'), nil, keyStroke('', 'left')),
      new('', 'j', keyStroke('', 'down'), nil, keyStroke('', 'down')),
      new('', 'k', keyStroke('', 'up'), nil, keyStroke('', 'up')),
      new('', 'l', keyStroke('', 'right'), nil, keyStroke('', 'right')),
      new(one, 'escape', vim.unbind)
    }

    bind(one, 'escape', vim.bind)

    -- Preview.app
    local wf = hs.window.filter
    wf.new {'Preview', 'Finder'}
      :subscribe(wf.windowFocused, vim.bind)
      :subscribe(wf.windowUnfocused, vim.unbind)

    -- Window Chooser
    function choices()
      local choices = {}
      for k, v in ipairs(hs.window.orderedWindows()) do
        local text = v:title() == '' and v:application():name() or v:title() .. ' - ' .. v:application():name(),
        table.insert(choices, {text = text, id = v:id()})
      end
      table.insert(choices, 1, table.remove(choices))
      return choices
    end

    local chooser = hs.chooser.new(function(c) hs.window.get(c.id):focus() end)
      :choices(choices)

    bind('cmd', '`', function() chooser:refreshChoicesCallback():query(''):show() end)

    -- End

    hs.alert('Config loaded.')
