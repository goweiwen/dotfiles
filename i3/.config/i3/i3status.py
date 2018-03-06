from subprocess import call
from i3pystatus import Status

status = Status(standalone=True)

status.register("clock",
    format="%a %-d %b %H:%M",
    interval=60,)

# # Shows your CPU temperature, if you have a Intel CPU
# # status.register("temp",
# #     format="{temp:.0f}°C",
# #     interval=30,)

# status.register("battery",
#     format="{status} {percentage:.2f}%",
#     alert=True,
#     alert_percentage=10,
#     status={
#         "DIS": "↓",
#         "CHR": "↑",
#         "FULL": "=",
#     },)

# # Note: the network module requires PyPI package netifaces
# status.register("network",
#     interface="enp4s0",
#     format_up="{v4cidr}",
#     format_down="",)

# # Note: requires both netifaces and basiciw (for essid and quality)
# status.register("network",
#     interface="wlan0",
#     format_up="{essid} {quality:03.0f}%",
#     format_down="",)

# # Shows disk usage of /
# # Format:
# # 42/128G [86G]
# status.register("disk",
#     path="/",
#     format="{used}/{total}G [{avail}G]",)

# Note: requires alsaaudio from PyPI

status.register("pulseaudio",
    format="V:{volume}%",
    format_muted="V: M ",
    step=2,
    on_leftclick="volume_switch",)

# Shows mpd status
status.register("mpd",
    format="{status} {title}",
    status={
        "pause": "◾",
        "play": "♪",
        "stop": "◾",
#        "pause": "▷",
#        "play": "▶",
#        "stop": "◾",
    },)

status.register("shell",
    command="news-ticker",
    on_downscroll=":",
    on_rightclick="news-ticker-open",
    interval=30,)

status.run()