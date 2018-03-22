require '../utils'

local chunkc = '/usr/local/bin/chunkc'

-- Reload
bind('ctrl-cmd-alt', 'r', function()
    hs.alert('Reloading config...')
    hs.reload()
    hs.execute('sh ~/.chunkwmrc')
  end)

-- Focus window
bind(one, 'h', execute(chunkc .. ' tiling:window --focus west'))
bind(one, 'j', execute(chunkc .. ' tiling:window --focus south'))
bind(one, 'k', execute(chunkc .. ' tiling:window --focus north'))
bind(one, 'l', execute(chunkc .. ' tiling:window --focus east'))

-- Move window
bind(two, 'h', execute(chunkc .. ' tiling:window --warp west'))
bind(two, 'j', execute(chunkc .. ' tiling:window --warp south'))
bind(two, 'k', execute(chunkc .. ' tiling:window --warp north'))
bind(two, 'l', execute(chunkc .. ' tiling:window --warp east'))


-- Move Space/Monitor
bind('ctrl-shift', 'up', execute(chunkc .. ' tiling:window --send-to-monitor next'))
bind('ctrl-shift', 'down', execute(chunkc .. ' tiling:window --send-to-monitor prev'))
bind('ctrl-shift', 'left', compose(
       execute(chunkc .. ' tiling:window --send-to-desktop prev'),
       keyStroke('ctrl', 'left')))
bind('ctrl-shift', 'right', compose(
       execute(chunkc .. ' tiling:window --send-to-desktop next'),
       keyStroke('ctrl', 'right')))

for i = 1, 9 do
  bind('ctrl-shift', tostring(i), compose(
         execute(chunkc .. ' tiling:window --send-to-desktop ' .. tostring(i)),
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
main
  :bind('', 'x', exitAfter(main, execute(chunkc .. ' tiling:desktop --mirror vertical')))
  :bind('', 'y', exitAfter(main, execute(chunkc .. ' tiling:desktop --mirror horizontal')))
  :bind('', 'r', exitAfter(main, execute(chunkc .. ' tiling:desktop --rotate 90')))
  :bind('', 'h', execute(chunkc .. ' tiling:window --use-temporary-ratio 0.1 --adjust-window-edge west'))
  :bind('', 'j', execute(chunkc .. ' tiling:window --use-temporary-ratio 0.1 --adjust-window-edge south'))
  :bind('', 'k', execute(chunkc .. ' tiling:window --use-temporary-ratio 0.1 --adjust-window-edge north'))
  :bind('', 'l', execute(chunkc .. ' tiling:window --use-temporary-ratio 0.1 --adjust-window-edge east'))
  :bind('shift', 'h', execute(chunkc .. ' tiling:window --use-temporary-ratio -0.1 --adjust-window-edge east'))
  :bind('shift', 'j', execute(chunkc .. ' tiling:window --use-temporary-ratio -0.1 --adjust-window-edge north'))
  :bind('shift', 'k', execute(chunkc .. ' tiling:window --use-temporary-ratio -0.1 --adjust-window-edge south'))
  :bind('shift', 'l', execute(chunkc .. ' tiling:window --use-temporary-ratio -0.1 --adjust-window-edge west'))
  :bind('', 'escape', function() hs.alert('Esc'); main:exit() end)
  :bind('', 'b', exitAfter(main, execute(chunkc .. ' tiling:desktop --toggle offset')))
  :bind('', 'm', exitAfter(main, execute(chunkc .. ' tiling:window --toggle fullscreen')))
  :bind('', 'f', exitAfter(main, execute(chunkc .. ' tiling:window --toggle native-fullscreen')))
  :bind('', 'z', exitAfter(main, execute(chunkc .. ' tiling:window --toggle parent')))
  :bind('', 'e', exitAfter(main, execute(chunkc .. ' tiling:window --toggle split')))
  :bind('', 't', exitAfter(main, execute(chunkc .. ' tiling:window --toggle float')))
  :bind('', 's', exitAfter(main, execute(chunkc .. ' tiling::window --toggle sticky; chunkc tiling::window --warp-floating pip-right')))

-- Next layout
local layouts = {'bsp', 'monocle', 'float'}
local layout = 1
main:bind('', 'space', function()
       layout = layout % #layouts + 1
       hs.execute(chunkc .. ' tiling:desktop --layout ' .. layouts[layout], true)
       hs.alert(layouts[layout]:gsub('^%l', string.upper))
                   end, exit)
