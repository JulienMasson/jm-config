#!/bin/sh

# dbus
export $(dbus-launch)

# check if we are docked
DOCKED=`xrandr -q | grep -c "DP-1-1 connected"`
if [ "$DOCKED" -eq 1 ]; then
    sh ~/.screenlayout/docked.sh
else
    sh ~/.screenlayout/laptop.sh

    # Change ctrl
    setxkbmap -option ctrl:swapcaps
    setxkbmap -option ctrl:nocaps
fi

# Start jwm
exec jwm
