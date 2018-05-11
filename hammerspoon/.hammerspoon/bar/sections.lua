require 'bar.config'

bar = c.new(dimensions)
   :canvasDefaultFor("padding", 10)
   :behavior("canJoinAllSpaces")
   :show()

-- Background
function background(bar)
   bar:appendElements(
      {
         action = "fill",
         fillColor = bg,
         frame = dimensions,
         type = "rectangle",
         withShadow = true,
   })
end

function calculateOffset(bar)
   local elements = bar:canvasElements()
   local i = bar:elementCount()
   if i == 0 then
      return 0
   end
   local bounds = bar:elementBounds(i)
   return bounds.x + bounds.w
end

-- Current Window
function title(bar)
   local i = bar:elementCount() + 1
   bar[i] = {
      action = "fill",
      frame = { x = calculateOffset(bar), y = 0, w = dimensions.w, h = dimensions.h },
      type = "text",
      text = hs.window.focusedWindow():application():title(),
      textFont = fontBold,
      textSize = 14
   }

   hs.application.watcher.new(function(appName, event, app)
         if event == hs.application.watcher.activated then
            bar[i].text = appName
         end
   end):start()
end

-- Battery
function battery(bar)
   local i = bar:elementCount() + 1
   bar[i] = {
      action = "fill",
      frame = dimensions,
      type = "text",
      text = hs.battery.percentage() .. "%",
      textFont = font,
      textSize = 14,
      textAlignment = "right"
   }

   hs.battery.watcher.new(function()
         bar[i].text = hs.battery.percentage() .. "%"
   end):start()
end

-- Date
function date(bar)
   local i = bar:elementCount() + 1
   bar[i] = {
      action = "fill",
      frame = dimensions,
      type = "text",
      text = "1",
      textFont = font,
      textSize = 14,
      textAlignment = "right"
   }

   hs.timer.doEvery(1, function()
                       bar[i].text = bar[i].text + 1
   end)
end

-- Wifi
function wifi(bar)
   local i = bar:elementCount() + 1
   bar[i]= {
      action = "fill",
      frame = dimensions,
      type = "text",
      text = hs.wifi.currentNetwork(),
      textFont = font,
      textSize = 14,
      textAlignment = "right"
   }

   hs.wifi.watcher.new(function(watcher, message, interface)
         bar[i].text = hs.wifi.currentNetwork(interface) or ''
   end):watchingFor("SSIDChange"):start()
end

-- Spaces
function spaces(bar)
   local _spaces = require 'hs._asm.undocumented.spaces'

   local i = bar:elementCount() + 1

   local spaceIndexToName = {
      [1] = "home",
      [5] = "www",
      [6] = "dev",
      [7] = "comm",
      [8] = "misc",
      [9] = "hold",
   }

   bar[i] = {
      action = "fill",
      frame = { x = 0, y = 0, w = 40, h = dimensions.h },
      type = "text",
      text = spaceIndexToName[_spaces.activeSpace()] or "misc",
      textFont = font,
      textSize = 14,
      textAlignment = "left"
   }

   hs.spaces.watcher.new(function()
         bar[i].text = spaceIndexToName[_spaces.activeSpace()] or "misc"
   end):start()
end
