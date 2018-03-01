#!/bin/sh

# change ctrl
setxkbmap -option ctrl:swapcaps
setxkbmap -option ctrl:nocaps

# num lock off
numlockx off

exec jwm
