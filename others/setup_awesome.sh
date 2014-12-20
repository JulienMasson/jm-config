#!/bin/bash

AWESOME_HOME="~/.config/awesome"
[ ! -d $AWESOME_HOME ] && mkdir -p $AWESOME_HOME

cp ~/jm-config/awesome/rc.lua ~/.config/awesome/
cp -r ~/jm-config/awesome/debian ~/.config/awesome/
