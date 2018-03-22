require '../utils'

local apps = {
  {'q', 'Finder'},
  {'w', 'Safari Technology Preview'},
  {'e', 'Emacs'},
  {'r', 'iTerm2'},
  {'a', 'Telegram'},
  {'s', 'WhatsApp'},
  {'d', 'Discord'},
  {'f', 'Slack'},
  {'z', 'Fantastical'},
  {'x', 'Boxy'},
}

local main = hs.hotkey.modal.new(one, 'r')

local summary = ''
for _, app in ipairs(apps) do
  summary = summary .. app[1] ..' - ' .. app[2] .. '\n'
  main:bind('', app[1], exitAfter(main, execute('open -a "' .. app[2] .. '"')))
end
summary = string.sub(summary, 1, -2)

function main:entered()
  self.alert = hs.alert(summary, 10)
end

function main:exited()
  if self.alert ~= nil then
    hs.alert.closeSpecific(self.alert)
    self.alert = nil
  end
end

main:bind('', 'escape', function() hs.alert('Esc'); main:exit() end)
