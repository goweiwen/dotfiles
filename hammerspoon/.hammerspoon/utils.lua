bind = hs.hotkey.bind
new = hs.hotkey.new

one = 'ctrl-cmd'
two = 'ctrl-shift-cmd'
three = 'ctrl-alt'
four = 'ctrl-alt-shift'

function execute(cmd, with_user_env)
  return function() io.popen(cmd, with_user_env) end
end

local delay = hs.eventtap.keyRepeatInterval()
function keyStroke(mod, key)
  return function() hs.timer.doAfter(0.01, function() hs.eventtap.keyStroke(mod, key, delay) end) end
end

function compose(...)
  local fns = { ... }
  return function()
    for _, fn in ipairs(fns) do
      fn()
    end
  end
end

function exitAfter(mode, fn)
  return function() fn() mode:exit() end
end
