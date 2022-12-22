-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local lain = require("lain")
lain.layout.equalarea = require("equalarea")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify({ preset = naughty.config.presets.critical,
    title = "Oops, there were errors during startup!",
    text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal("debug::error", function (err)
    -- Make sure we don't go into an endless error loop
    if in_error then return end
    in_error = true

    naughty.notify({ preset = naughty.config.presets.critical,
      title = "Oops, an error happened!",
      text = tostring(err) })
    in_error = false
  end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.

local tags = {
  {
    name = "  ",
    layout = awful.layout.suit.floating,
  },
  {
    name = "  ",
    layout = lain.layout.centerwork,
  },
  {
    name = "  ",
    layout = awful.layout.suit.floating,
  },
  {
    name = "  ",
    layout = lain.layout.termfair.center,
  },
  {
    name = "  ",
    layout = awful.layout.suit.floating,
  },
}

beautiful.init(os.getenv("HOME") .. "/.config/awesome/themes/catppuccin/theme.lua")
naughty.config.defaults.margin = beautiful.notification_margin
naughty.config.defaults.position = "top_middle"

-- This is used later as the default terminal and editor to run.
terminal = "kitty"
editor = os.getenv("EDITOR") or "neovide"
editor_cmd = terminal .. " -e vim"

files = "nautilus"
browser = "firefox"
locker = "i3lock"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"
altkey = "Mod1"

awful.util.terminal = terminal
awful.layout.layouts = {
  lain.layout.equalarea,
  awful.layout.suit.tile,
  lain.layout.termfair.center,
  lain.layout.centerwork,
  awful.layout.suit.floating,
  awful.layout.max,
}

lain.layout.termfair.nmaster           = 3
lain.layout.termfair.ncol              = 1
lain.layout.termfair.center.nmaster    = 3
lain.layout.termfair.center.ncol       = 1
lain.layout.cascade.tile.offset_x      = 2
lain.layout.cascade.tile.offset_y      = 32
lain.layout.cascade.tile.extra_padding = 5
lain.layout.cascade.tile.nmaster       = 5
lain.layout.cascade.tile.ncol          = 2
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
  { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
  { "manual", terminal .. " -e man awesome" },
  { "edit config", editor_cmd .. " " .. awesome.conffile },
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
  { "open terminal", terminal }
}
})

-- }}}

-- Widgets
-- Clock
local mytextclock = wibox.widget.textclock("<b>%a %d %b %H:%M</b>")

-- MPris
local mympris = awful.widget.watch("playerctl metadata --player=chromium,firefox --format '{{ status }} {{ artist }} - {{ title }}'", 1, function(widget, stdout)
  if stdout == "No players found" then
    widget.visible = false
  else
    widget.visible = true
    stdout = stdout:gsub("^Paused", "<span font-size=\"14pt\" rise=\"-2pt\"></span> ")
    stdout = stdout:gsub("^Playing", "<span font-size=\"14pt\" rise=\"-2pt\"></span> ")
    widget:set_markup_silently(stdout)
  end
end)
mympris.valign = "center"
mympris:buttons(gears.table.join(
  awful.button({ }, 1, function () awful.util.spawn("playerctl --player=chromium,firefox play-pause") end),
  awful.button({ }, 3, function () awful.util.spawn("playerctl --player=chromium,firefox next") end),
  awful.button({ }, 4, function () awful.util.spawn("playerctl --player=chromium,firefox volume +5") end),
  awful.button({ }, 5, function () awful.util.spawn("playerctl --player=chromium,firefox volume -5") end)))

-- Volume
local mypulseaudio = wibox.widget {
  markup = 'Audio',
  widget = wibox.widget.textbox,
}
function mypulseaudio:update()
  awful.spawn.easy_async("pamixer --get-volume-human", function(stdout)
    if stdout == "muted\n" then
      self:set_text("ﱝ Muted")
    else
      local volume = tonumber(stdout:sub(1, -3))
      if volume < 33 then
        self:set_text(" " .. stdout)
      elseif volume < 66 then
        self:set_text(" " .. stdout)
      else
        self:set_text(" " .. stdout)
      end
    end
  end)
end
mypulseaudio:update()
mypulseaudio:buttons(gears.table.join(
  awful.button({ }, 1, function () awful.util.spawn("pamixer -t") mypulseaudio:update() end),
  awful.button({ }, 3, function () awful.util.spawn("pavucontrol") mypulseaudio:update() end),
  awful.button({ }, 4, function () awful.util.spawn("pamixer -i 5") mypulseaudio:update() end),
  awful.button({ }, 5, function () awful.util.spawn("pamixer -d 5") mypulseaudio:update() end)))

-- Keyboard map indicator and switcher
local mykeyboardlayout = awful.widget.keyboardlayout()

local mysystray = wibox.widget.systray()
mysystray:set_base_size(20)

local function set_wallpaper(s)
  -- Wallpaper
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    -- If wallpaper is a function, call it with the screen
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper
  set_wallpaper(s)

  -- Each screen has its own tag table.
  for i, tag in ipairs(tags) do
    awful.tag.add(tag.name, {
      layout = tag.layout,
      screen = s,
    })
  end

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
    awful.button({ }, 1, function () awful.layout.inc( 1) end),
    awful.button({ }, 3, function () awful.layout.inc(-1) end),
    awful.button({ }, 4, function () awful.layout.inc( 1) end),
    awful.button({ }, 5, function () awful.layout.inc(-1) end)))

  -- Create a taglist widget
  awful.util.taglist_buttons = awful.util.table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ modkey }, 1, function(t)
        if client.focus then
          client.focus:move_to_tag(t)
        end
    end),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, function(t)
        if client.focus then
          client.focus:toggle_tag(t)
        end
    end),
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
  )
  s.mytaglist = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = awful.util.taglist_buttons
  }

  -- Create a tasklist widget
  awful.util.tasklist_buttons = awful.util.table.join(
    awful.button({ }, 1, function (c)
        if c == client.focus then
          c.minimized = true
        else
          c.minimized = false
          if not c:isvisible() and c.first_tag then
            c.first_tag:view_only()
          end
          client.focus = c
          c:raise()
        end
    end),
    awful.button({ }, 2, function (c) c:kill() end),
    awful.button({ }, 3, function ()
        local instance = nil

        return function ()
          if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
          else
            instance = awful.menu.clients({theme = {width = dpi(250)}})
          end
        end
    end),
    awful.button({ }, 4, function () awful.client.focus.byidx(1) end),
    awful.button({ }, 5, function () awful.client.focus.byidx(-1) end)
  )
  s.mytasklist = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = awful.util.tasklist_buttons,
    layout = {
      layout = wibox.layout.fixed.horizontal,
      spacing = 8
    }
  }

  -- Create the wibox
  s.mywibox = awful.wibar({ position = "top", screen = s })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.stack,
    valign = "center",
    {
      layout = wibox.layout.align.horizontal,
      { -- Left widgets
        layout = wibox.layout.fixed.horizontal,
        valign = "center",
        spacing = 12,
        s.mytaglist,
        s.mypromptbox,
        s.mytasklist,
      },
      nil, -- Middle widget
      { -- Right widgets
        layout = wibox.layout.fixed.horizontal,
        valign = "center",
        spacing = 12,
        mympris,
        mypulseaudio,
        -- mykeyboardlayout,
        mysystray,
        s.mylayoutbox,
      },
    },
    {
      {
        {
          mytextclock,
          left = 24,
          right = 24,
          widget = wibox.container.margin,
        },
        bg = beautiful.bg_normal,
        widget = wibox.container.background,
      },
      valign = "center",
      halign = "center",
      layout = wibox.container.place,
    }
  }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
  awful.button({ }, 3, function () mymainmenu:toggle() end),
  awful.button({ }, 4, awful.tag.viewnext),
  awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(
  awful.key({ modkey,           }, "/",      hotkeys_popup.show_help,
    {description="show help", group="awesome"}),
  awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
    {description = "view previous", group = "tag"}),
  awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
    {description = "view next", group = "tag"}),
  awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
    {description = "go back", group = "tag"}),

  awful.key({ modkey }, "j",
    function()
      awful.client.focus.global_bydirection("down")
      if client.focus then client.focus:raise() end
    end,
    {description = "focus down", group = "client"}),
  awful.key({ modkey }, "k",
    function()
      awful.client.focus.global_bydirection("up")
      if client.focus then client.focus:raise() end
    end,
    {description = "focus up", group = "client"}),
  awful.key({ modkey }, "h",
    function()
      awful.client.focus.global_bydirection("left")
      if client.focus then client.focus:raise() end
    end,
    {description = "focus left", group = "client"}),
  awful.key({ modkey }, "l",
    function()
      awful.client.focus.global_bydirection("right")
      if client.focus then client.focus:raise() end
    end,
    {description = "focus right", group = "client"}),

  -- Menu
  awful.key({ modkey, "Shift"   }, "w", function () mymainmenu:show() end,
    {description = "show main menu", group = "awesome"}),

  -- Layout manipulation
  awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.bydirection("down") end,
    {description = "swap down", group = "client"}),
  awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.bydirection("up") end,
    {description = "swap up", group = "client"}),
  awful.key({ modkey, "Shift" }, "h", function () awful.client.swap.bydirection("left") end,
    {description = "swap left", group = "screen"}),
  awful.key({ modkey, "Shift" }, "l", function () awful.client.swap.bydirection("right") end,
    {description = "swap right", group = "screen"}),
  awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
    {description = "jump to urgent client", group = "client"}),
  awful.key({ altkey,           }, "Tab",
    function ()
      awful.client.focus.byidx(-1)
      if client.focus then
        client.focus:raise()
      end
    end),

  awful.key({ altkey, "Shift"   }, "Tab",
    function ()
      awful.client.focus.byidx(1)
      if client.focus then
        client.focus:raise()
      end
    end),

  -- libspeedhack
  awful.key({ }, "XF86MonBrightnessUp", function () os.execute("echo 1 > /tmp/speedhack_pipe") end,
    {description = "1x speed", group = "hotkeys"}),
  awful.key({ }, "XF86MonBrightnessDown", function () os.execute("echo 4 > /tmp/speedhack_pipe") end,
    {description = "4x speed", group = "hotkeys"}),

  -- Screen brightness
  awful.key({ }, "XF86MonBrightnessUp", function () os.execute("xbacklight -inc 10") end,
    {description = "+10%", group = "hotkeys"}),
  awful.key({ }, "XF86MonBrightnessDown", function () os.execute("xbacklight -dec 10") end,
    {description = "-10%", group = "hotkeys"}),

  -- Multimedia control
  awful.key({ }, "XF86AudioPlay", function () awful.util.spawn("playerctl play-pause") end),
  awful.key({ }, "XF86AudioNext", function () awful.util.spawn("playerctl next") end),
  awful.key({ }, "XF86AudioPrev", function () awful.util.spawn("playerctl previous") end),
  awful.key({ }, "XF86AudioRaiseVolume", function () awful.util.spawn("pamixer -i 5") mypulseaudio:update() end),
  awful.key({ }, "XF86AudioLowerVolume", function () awful.util.spawn("pamixer -d 5") mypulseaudio:update() end),
  awful.key({ }, "XF86AudioMute", function () awful.util.spawn("pamixer -t") mypulseaudio:update() end),

  -- Take a screenshot
  awful.key({ modkey }, "s", function() os.execute("maim -- ~/Pictures/Screenshots/" .. os.date("%Y-%m-%d_%H-%M") .. ".png && pw-play /usr/share/sounds/freedesktop/stereo/camera-shutter.oga") end,
    {description = "take a screenshot", group = "hotkeys"}),
  awful.key({ modkey, "Shift" }, "s", function() os.execute("maim --select -- ~/Pictures/Screenshots/" .. os.date("%Y-%m-%d_%H-%M") .. ".png && pw-play /usr/share/sounds/freedesktop/stereo/camera-shutter.oga") end,
    {description = "take a screenshot (selection)", group = "hotkeys"}),
  awful.key({ modkey, "Alt" }, "s", function() os.execute("maim --window $(xdotool getactivewindow) -- ~/Pictures/Screenshots/" .. os.date("%Y-%m-%d_%H-%M") .. ".png && pw-play /usr/share/sounds/freedesktop/stereo/camera-shutter.oga") end,
    {description = "take a screenshot (window)", group = "hotkeys"}),
  awful.key({ modkey, "Control" }, "s", function() os.execute("maim | xclip -selection clipboard -target image/png && pw-play /usr/share/sounds/freedesktop/stereo/camera-shutter.oga") end,
    {description = "take a screenshot to clipboard", group = "hotkeys"}),
  awful.key({ modkey, "Control", "Shift" }, "s", function() os.execute("maim --select | xclip -selection clipboard -target image/png && pw-play /usr/share/sounds/freedesktop/stereo/camera-shutter.oga") end,
    {description = "take a screenshot to clipboard (selection)", group = "hotkeys"}),
  awful.key({ modkey, "Control", "Alt" }, "s", function() os.execute("maim --window $(xdotool getactivewindow) | xclip -selection clipboard -target image/png && pw-play /usr/share/sounds/freedesktop/stereo/camera-shutter.oga") end,
    {description = "take a screenshot to clipboard (window)", group = "hotkeys"}),

  -- Copy primary to clipboard (terminals to gtk)
  awful.key({ modkey }, "c", function () awful.spawn.with_shell("xsel | xsel -i -b") end,
    {description = "copy terminal to gtk", group = "hotkeys"}),
  -- Copy clipboard to primary (gtk to terminals)
  awful.key({ modkey }, "v", function () awful.spawn.with_shell("xsel -b | xsel") end,
    {description = "copy gtk to terminal", group = "hotkeys"}),
  -- Programs
  awful.key({ modkey }, "w", function () awful.spawn.raise_or_spawn(browser, { class = "Firefox" }) end,
    {description = "run browser", group = "launcher"}),
  awful.key({ modkey }, "e", function () awful.spawn(files) end,
    {description = "nautilus", group = "launcher"}),

  -- Standard program
  awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
    {description = "open a terminal", group = "launcher"}),
  awful.key({ modkey, "Control" }, "r", awesome.restart,
    {description = "reload awesome", group = "awesome"}),

  awful.key({ modkey, "Control" }, "l",     function () awful.tag.incmwfact( 0.05)          end,
    {description = "increase master width factor", group = "layout"}),
  awful.key({ modkey, "Control" }, "h",     function () awful.tag.incmwfact(-0.05)          end,
    {description = "decrease master width factor", group = "layout"}),
  awful.key({ modkey, "Control", "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
    {description = "increase the number of master clients", group = "layout"}),
  awful.key({ modkey, "Control", "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
    {description = "decrease the number of master clients", group = "layout"}),
  awful.key({ modkey, altkey }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
    {description = "increase the number of columns", group = "layout"}),
  awful.key({ modkey, altkey }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
    {description = "decrease the number of columns", group = "layout"}),
  awful.key({ modkey }, "z", function () awful.layout.inc( 1)                end,
    {description = "select next", group = "layout"}),
  awful.key({ modkey, "Shift" }, "z", function () awful.layout.inc(-1)                end,
    {description = "select previous", group = "layout"}),

  awful.key({ modkey, "Control" }, "n",
    function ()
      local c = awful.client.restore()
      -- Focus restored client
      if c then
        c:emit_signal(
          "request::activate", "key.unminimize", {raise = true}
        )
      end
    end,
    {description = "restore minimized", group = "client"}),

  -- Prompt
  awful.key({ modkey }, "space", function() awful.spawn("rofi -show drun") end,
    {description = "app launcher", group = "launcher"}),
  awful.key({ modkey, "Shift" }, "q", function() awful.spawn("rofi -show powermenu -modi powermenu:~/.local/bin/rofi-power-menu") end,
    {description = "power menu", group = "launcher"}),

  awful.key({ modkey }, "x",
    function ()
      awful.prompt.run {
        prompt       = "Run Lua code: ",
        textbox      = awful.screen.focused().mypromptbox.widget,
        exe_callback = awful.util.eval,
        history_path = awful.util.get_cache_dir() .. "/history_eval"
      }
    end,
    {description = "lua execute prompt", group = "awesome"}),
  -- Menubar
  awful.key({ modkey }, "p", function() awful.util.spawn("rofi -show window") end,
    {description = "jump windows", group = "launcher"})
)

clientkeys = gears.table.join(
  awful.key({ modkey,           }, "f",
    function (c)
      c.fullscreen = not c.fullscreen
      c:raise()
    end,
    {description = "toggle fullscreen", group = "client"}),
  awful.key({ modkey }, "q", function (c) c:kill() end,
    {description = "close", group = "client"}),
  awful.key({ altkey }, "F4", function (c) c:kill() end,
    {description = "close", group = "client"}),
  awful.key({ modkey, "Shift" }, "f", awful.client.floating.toggle,
    {description = "toggle floating", group = "client"}),
  awful.key({ modkey, "Shift" }, "`", function (c) c.sticky = not c.sticky end,
    {description = "toggle sticky", group = "client"}),
  awful.key({ modkey, "Shift" }, "t", function (c) c.ontop = not c.ontop end,
    {description = "toggle keep on top", group = "client"}),
  awful.key({ modkey, "Shift" }, "m", function (c) c:swap(awful.client.getmaster()) end,
    {description = "move to master", group = "client"}),
  awful.key({ modkey, }, "o", function (c) c:move_to_screen() end,
    {description = "move to screen", group = "client"}),
  awful.key({ modkey, }, "n",
    function (c)
      -- The client currently has the input focus, so it cannot be
      -- minimized, since minimized clients can't have the focus.
      c.minimized = true
    end ,
    {description = "minimize", group = "client"}),
  awful.key({ modkey,           }, "m",
    function (c)
      c.maximized = not c.maximized
      c:raise()
    end ,
    {description = "(un)maximize", group = "client"}),
  awful.key({ modkey, "Control" }, "m",
    function (c)
      c.maximized_vertical = not c.maximized_vertical
      c:raise()
    end ,
    {description = "(un)maximize vertically", group = "client"}),
  awful.key({ modkey, "Control"   }, "n",
    function (c)
      c.maximized_horizontal = not c.maximized_horizontal
      c:raise()
    end ,
    {description = "(un)maximize horizontally", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
  globalkeys = gears.table.join(globalkeys,
    -- View tag only.
    awful.key({ modkey }, "#" .. i + 9,
      function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
          tag:view_only()
        end
      end,
      {description = "view tag #"..i, group = "tag"}),
    -- Toggle tag display.
    awful.key({ modkey, "Control" }, "#" .. i + 9,
      function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
          awful.tag.viewtoggle(tag)
        end
      end,
      {description = "toggle tag #" .. i, group = "tag"}),
    -- Move client to tag.
    awful.key({ modkey, "Shift" }, "#" .. i + 9,
      function ()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:move_to_tag(tag)
          end
        end
      end,
      {description = "move focused client to tag #"..i, group = "tag"}),
    -- Toggle tag on focused client.
    awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
      function ()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:toggle_tag(tag)
          end
        end
      end,
      {description = "toggle focused client on tag #" .. i, group = "tag"})
  )
end

clientbuttons = gears.table.join(
  awful.button({ }, 1, function (c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
  end),
  awful.button({ modkey }, 1, function (c)
    awful.mouse.client.floating = true
    c:emit_signal("request::activate", "mouse_click", {raise = true})
    awful.mouse.client.move(c)
  end),
  awful.button({ modkey }, 3, function (c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
    awful.mouse.client.resize(c)
  end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
  -- All clients will match this rule.
  { rule = { },
    properties = { border_width = beautiful.border_width,
      border_color = beautiful.border_normal,
      focus = awful.client.focus.filter,
      raise = true,
      keys = clientkeys,
      buttons = clientbuttons,
      screen = awful.screen.preferred,
      placement = awful.placement.no_overlap+awful.placement.no_offscreen
    }
  },

  -- Polybar
  { rule = { class = "Polybar" },
    properties = { border_width = 0, focusable = false }
  },

  -- Fullscreen
  { rule_any = {
    class = {
      "Minecraft",
      "PokeWilds",
      "Eternum",
    },
  }, properties = { tag = tags[3].name, switchtotag=  true, fullscreen = true }
  },

  -- Floating clients.
  { rule_any = {
    instance = {
      "DTA",  -- Firefox addon DownThemAll.
      "copyq",  -- Includes session name in class.
      "pinentry",
      "origin.exe",
      "Battle.net.exe",
    },
    class = {
      "Pavucontrol",
      "Arandr",
      "Blueman-manager",
      "Gpick",
      "Kruler",
      "MessageWin",  -- kalarm.
      "Sxiv",
      "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
      "Wpa_gui",
      "veromix",
      "xtightvncviewer"},

    -- Note that the name property shown in xprop might be set slightly after creation of the client
    -- and the name shown there might not match defined rules here.
    name = {
      "Event Tester",  -- xev.
    },
    role = {
      "AlarmWindow",  -- Thunderbird's calendar.
      "ConfigManager",  -- Thunderbird's about:config.
      "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
    }
  }, properties = { floating = true, placement = awful.placement.centered }},

  -- Add titlebars to normal clients and dialogs
  { rule_any = {type = {  "dialog" }
  }, properties = { titlebars_enabled = true }
  },

  -- Set Firefox to always map on the tag named "2" on screen 1.
  { rule = { class = "Firefox" },
    properties = { tag = tags[2].name, switchtotag = true } },
  { rule = { class = "firefox", name = "Picture-in-Picture" },
    properties = { floating = true, sticky = true, ontop = true } },
  { rule = { class = "TelegramDesktop" },
    properties = { tag = tags[4].name, switchtotag = true } },
  { rule = { class = "discord" },
    properties = { tag = tags[4].name, switchtotag = true } },
  { rule = { class = "prismlauncher" },
    properties = { tag = tags[3].name, switchtotag = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
  -- Set the windows at the slave,
  -- i.e. put it at the end of others instead of setting it master.
  if not awesome.startup then awful.client.setslave(c) end

  if awesome.startup
    and not c.size_hints.user_position
    and not c.size_hints.program_position then
    -- Prevent clients from being unreachable after screen count changes.
    awful.placement.no_offscreen(c)
  end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
  -- buttons for the titlebar
  local buttons = gears.table.join(
    awful.button({ }, 1, function()
      c:emit_signal("request::activate", "titlebar", {raise = true})
      awful.mouse.client.move(c)
    end),
    awful.button({ }, 3, function()
      c:emit_signal("request::activate", "titlebar", {raise = true})
      awful.mouse.client.resize(c)
    end)
  )

  awful.titlebar(c) : setup {
    { -- Left
      awful.titlebar.widget.iconwidget(c),
      buttons = buttons,
      layout  = wibox.layout.fixed.horizontal
    },
    { -- Middle
      { -- Title
        align  = "center",
        widget = awful.titlebar.widget.titlewidget(c)
      },
      buttons = buttons,
      layout  = wibox.layout.flex.horizontal
    },
    { -- Right
      awful.titlebar.widget.floatingbutton (c),
      awful.titlebar.widget.maximizedbutton(c),
      awful.titlebar.widget.stickybutton   (c),
      awful.titlebar.widget.ontopbutton    (c),
      awful.titlebar.widget.closebutton    (c),
      layout = wibox.layout.fixed.horizontal()
    },
    layout = wibox.layout.align.horizontal
  }
end)

-- Rounded corners
-- Applied with picom
--[[
function apply_rounded_corners(c)
    if c.border_width ~= 0 then
        if c.fullscreen or c.maximized then
            c.shape = gears.shape.rectangle
        else
            c.shape = function(cr, w, h)
                gears.shape.rounded_rect(cr, w, h, 15)
            end
        end
    end
end

client.connect_signal("manage", apply_rounded_corners)
client.connect_signal("property::fullscreen", apply_rounded_corners)
client.connect_signal("property::maximized", apply_rounded_corners)
]]--

-- Notification sounds
function naughty.config.notify_callback(args)
  awful.util.spawn("pw-play /usr/share/sounds/freedesktop/stereo/message.oga")
  return args
end

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Autostart Applications
-- awful.spawn.once("easyeffects --gapplication-service")
awful.spawn.once("picom --experimental-backends")
-- awful.spawn.with_shell("polybar all")
awful.spawn.once("uim-xim")
awful.spawn.with_shell("~/.fehbg")
awful.spawn.with_shell("killall uim-toolbar-gtk3-systray; uim-toolbar-gtk3-systray")
awful.spawn.once("kdeconnect-indicator")
awful.spawn.once('mapwacom --device-regex="Wacom Intuos BT M Pen stylus" -s "DP-4"')
