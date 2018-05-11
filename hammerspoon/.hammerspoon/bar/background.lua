c = require 'hs.canvas'
require 'utils'
require 'bar.sections'
require 'bar.config'

bar = c.new(dimensions)
   :canvasDefaultFor("padding", 10)
   :behavior("canJoinAllSpaces")
   :show()

-- Toggle bar
bind(one, 'b', function()
        if bar:isShowing() then
           bar:hide(0.2)
        else
           bar:show(0.2)
        end
end)

background(bar)

spaces(bar)
title(bar)

date(bar)
