# source ~/.xprofile

xrdb -merge "${HOME}/.Xresources"
xrdb -merge "${HOME}/.Xdefaults"
xmodmap "${HOME}/.Xmodmap"
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

export PATH=$HOME/bin:/usr/local/bin:$PATH
export EDITOR="vim"
export BROWSER="firefox-gtk3"

export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_SELECT=5
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
export XMODIFIERS='@im=uim'

# Mouse Sensitivity
xset m 0 0

# Monitors
xrandr --output DisplayPort-1 --primary --auto --right-of DVI-0 --output DVI-0 --auto --pos 0x0
xrandr --output DVI-0 --gamma 0.95:0.9:1
export SDL_VIDEO_FULLSCREEN_HEAD=1
export SDL_VIDEO_FULLSCREEN_DISPLAY=1

# Autostart
feh --bg-fill "${HOME}/Pictures/Wallpapers/highlands.jpg"
seq 0 1 | xargs -l1 -I@ compton -d :0.@ -b --config ~/.config/compton.conf
xsetroot -cursor_name left_ptr
alsactl restore
# xdotool behave_screen_edge top-left \
#     exec i3-dmenu-desktop --dmenu='rofi -dmenu' &

udiskie &
xbindkeys &
dunst &
rofi &
uim-xim &
mpd &
xautolock -time 10 -notify 30 -notifier "notify-send -t 30 -u critical 'Screen will lock in 30 seconds'" -locker "i3lock-wrapper -d" &

urxvt -name scratchpad &
#thunar --name scratchpad &
pidgin &

exec i3 -c ~/.config/i3/config
