#!/bin/sh

# dbus
export $(dbus-launch)

# check if we are in dual screen
DUAL_SCREEN=`xrandr -q | grep -E -e "VGA-1 connected" -e "HDMI-1 connected" | wc -l)`
if [ "$DUAL_SCREEN" -eq 2 ]; then
    xrandr --output HDMI1 --mode 1920x1200 --pos 1920x0 --rotate normal --output VIRTUAL1 --off --output DP1 --off --output eDP1 --off --output VGA1 --mode 1920x1200 --pos 0x0 --rotate normal
else
    xrandr --output HDMI1 --off --output VIRTUAL1 --off --output DP1 --off --output eDP1 --mode 1366x768 --pos 0x0 --rotate normal --output VGA1 --off
fi

# Set a background.
# xsetroot -solid grey20
feh --bg-scale /home/lab/Pictures/wallpaper.jpg

# Change ctrl
setxkbmap -option ctrl:swapcaps
setxkbmap -option ctrl:nocaps

# Start jwm
exec jwm
