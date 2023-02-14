local dpi = require("beautiful.xresources").apply_dpi

local iconfont = "TerminessTTF Nerd Font 13"

local theme = {}
theme.dir = os.getenv("HOME") .. "/.config/awesome/themes/catppuccin"
-- theme.wallpaper = os.getenv("HOME") .. "/Pictures/Wallpaper/default"
theme.font = "TerminessTTF Nerd Font 9"
theme.bold_font = "TerminessTTF Nerd Font 9"
theme.useless_gap = dpi(4)
theme.border_width = dpi(1)

theme.flamingo = "#f2cdcd"
theme.pink = "#f5c2e7"
theme.mauve = "#cba6f7"
theme.red = "#f38ba8"
theme.maroon = "#eba0ac"
theme.peach = "#fab387"
theme.yellow = "#f9e2af"
theme.green = "#a6e3a1"
theme.teal = "#94e2d5"
theme.sky = "#89dceb"
theme.sapphire = "#74c7ec"
theme.blue = "#89b4fa"
theme.lavender = "#b4befe"
theme.text = "#cdd6f4"
theme.subtext1 = "#bac2de"
theme.subtext0 = "#a6adc8"
theme.overlay2 = "#9399b2"
theme.overlay1 = "#7f849c"
theme.overlay0 = "#6c7086"
theme.surface2 = "#585b70"
theme.surface1 = "#45475a"
theme.surface0 = "#313244"
theme.base = "#1e1e2e"
theme.mantle = "#181825"
theme.crust = "#11111b"

theme.bg_normal = theme.base
theme.bg_focus = theme.mantle
theme.bg_urgent = theme.text
theme.bg_minimize = theme.base
theme.bg_systray = theme.crust

theme.fg_normal = theme.text
theme.fg_focus = theme.lavender
theme.fg_urgent = theme.red
theme.fg_minimize = theme.text

theme.border_normal = theme.base
theme.border_focus = theme.mauve
theme.border_marked = theme.pink

theme.menu_height = dpi(24)
theme.menu_width = dpi(130)
theme.menu_bg_focus = theme.mantle
theme.menu_border_color = theme.base
theme.menu_border_width = dpi(2)
theme.menu_submenu = "▶  "

theme.notification_icon_size = 64
theme.notification_width = 240
theme.notification_margin = 12
theme.notification_border_width = theme.border_width
theme.notification_border_color = theme.border_focus

theme.icon_theme = "/usr/share/icons/Papirus"

theme.taglist_font = iconfont

theme.tasklist_disable_icon                     = true

theme.awesome_icon                              = theme.dir .."/icons/awesome.png"
theme.menu_submenu_icon                         = theme.dir .. "/icons/submenu.png"
theme.taglist_squares_sel                       = theme.dir .. "/icons/square_unsel.png"
theme.taglist_squares_unsel                     = theme.dir .. "/icons/square_unsel.png"
theme.vol                                       = theme.dir .. "/icons/vol.png"
theme.vol_low                                   = theme.dir .. "/icons/vol_low.png"
theme.vol_no                                    = theme.dir .. "/icons/vol_no.png"
theme.vol_mute                                  = theme.dir .. "/icons/vol_mute.png"
theme.disk                                      = theme.dir .. "/icons/disk.png"
theme.ac                                        = theme.dir .. "/icons/ac.png"
theme.bat                                       = theme.dir .. "/icons/bat.png"
theme.bat_low                                   = theme.dir .. "/icons/bat_low.png"
theme.bat_no                                    = theme.dir .. "/icons/bat_no.png"
theme.play                                      = theme.dir .. "/icons/play.png"
theme.pause                                     = theme.dir .. "/icons/pause.png"
theme.stop                                      = theme.dir .. "/icons/stop.png"
theme.layout_tile                               = theme.dir .. "/icons/tile.png"
theme.layout_tileleft                           = theme.dir .. "/icons/tileleft.png"
theme.layout_tilebottom                         = theme.dir .. "/icons/tilebottom.png"
theme.layout_tiletop                            = theme.dir .. "/icons/tiletop.png"
theme.layout_fairv                              = theme.dir .. "/icons/fairv.png"
theme.layout_fairh                              = theme.dir .. "/icons/fairh.png"
theme.layout_spiral                             = theme.dir .. "/icons/spiral.png"
theme.layout_dwindle                            = theme.dir .. "/icons/dwindle.png"
theme.layout_max                                = theme.dir .. "/icons/max.png"
theme.layout_fullscreen                         = theme.dir .. "/icons/fullscreen.png"
theme.layout_magnifier                          = theme.dir .. "/icons/magnifier.png"
theme.layout_floating                           = theme.dir .. "/icons/floating.png"
theme.titlebar_close_button_focus               = theme.dir .. "/icons/titlebar/close_focus.png"
theme.titlebar_close_button_normal              = theme.dir .. "/icons/titlebar/close_normal.png"
theme.titlebar_ontop_button_focus_active        = theme.dir .. "/icons/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active       = theme.dir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive      = theme.dir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive     = theme.dir .. "/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_sticky_button_focus_active       = theme.dir .. "/icons/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active      = theme.dir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive     = theme.dir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive    = theme.dir .. "/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_floating_button_focus_active     = theme.dir .. "/icons/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active    = theme.dir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive   = theme.dir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive  = theme.dir .. "/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/titlebar/maximized_normal_inactive.png"
theme.layout_centerfair                         = theme.dir .. "/icons/centerfair.png"
theme.layout_termfair                           = theme.dir .. "/icons/termfair.png"
theme.layout_centerwork                         = theme.dir .. "/icons/centerwork.png"

return theme
