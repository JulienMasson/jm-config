#!/bin/sh

# Set a background.
# xsetroot -solid grey20
feh --bg-scale /home/lab/Pictures/wallpaper.jpg

# Change ctrl
setxkbmap -option ctrl:swapcaps
setxkbmap -option ctrl:nocaps

# Start jwm
exec jwm
