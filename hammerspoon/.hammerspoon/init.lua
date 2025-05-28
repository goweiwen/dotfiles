-- CJKVDict
hs.hotkey.bind('ctrl-cmd', '/', function()
    local clipboard = hs.pasteboard.getContents()
    hs.eventtap.keyStroke({ "cmd" }, "c")
    local query = hs.pasteboard.getContents()
    hs.pasteboard.setContents(clipboard)
    if query == nil or query == clipboard then
        return
    end
    query = hs.http.encodeForQuery(query)
    -- hs.urlevent.openURL("dict://" .. query)
    local url = "https://cjkvdict.com/?search=" .. query
    hs.osascript.applescript([[
        tell application "Arc"
            make new tab with properties {URL:"]] .. url .. [["}
            activate
        end tell
    ]])
end)

hs.hotkey.bind('ctrl-cmd-alt', 'r', function()
    hs.alert('Reloading config...')
    hs.timer.delayed.new(1, function() hs.reload() end)
end)

-- Caffeinate
hs.hotkey.bind('ctrl-cmd-alt', 'c', function()
    hs.caffeinate.toggle('system')
    if hs.caffeinate.get('system') then
        hs.alert('Caffeinated')
    else
        hs.alert('Decaff')
    end
end)

-- Autocompleter
;(function()
    local autocompleter = hs.chooser.new(function(selection)
        if selection == nil then
            return
        end

        if hs.eventtap.checkKeyboardModifiers().cmd then
            hs.pasteboard.setContents(selection.clipboard)
            hs.alert("Copied to clipboard")
        else
            hs.alert("Typing...")
            hs.eventtap.keyStrokes(selection.text)
        end
    end)

    local choices = {}
    local image = hs.image.imageFromASCII(" ")
    for line in io.lines("snippets.txt") do
        table.insert(choices, {
            ["text"] = line,
            ["image"] = image,
        })
    end
    autocompleter:choices(choices)

    hs.hotkey.bind('cmd-ctrl', 'L', function()
        autocompleter:show()
    end)
end)()

--[[
-- Alternate Command Tab

switcher = hs.window.switcher.new(hs.window.filter.new():setCurrentSpace(true))
switcher.ui.fontName = 'SF Pro'
switcher.ui.textColor = { 0, 0, 0, 1 }
switcher.ui.titleBackgroundColor = { 0, 0, 0, 0 }
switcher.ui.highlightColor = { 0.7, 0.7, 0.7, 0.9 }
switcher.ui.backgroundColor = { 0.95, 0.95, 0.95, 0.9 }
switcher.ui.showTitles = false
switcher.ui.showThumbnails = false
switcher.ui.showSelectedTitle = false
switcher.drawings.highlightRect:setRoundedRectRadii(16, 16)
switcher.drawings.selRect:setRoundedRectRadii(16, 16)
switcher.drawings.selIcon:clippingRectangle(hs.geometry.rect(0, 0, 0, 0))
function mapCmdTab(event)
    local flags = event:getFlags()
    local chars = event:getCharacters()
    if chars == "\t" and flags:containExactly { 'cmd' } then
        switcher:next()
        return true
    elseif chars == string.char(25) and flags:containExactly { 'cmd', 'shift' } then
        switcher:previous()
        return true
    end
end

tapCmdTab = hs.eventtap.new({ hs.eventtap.event.types.keyDown }, mapCmdTab)
tapCmdTab:start()
]]--
