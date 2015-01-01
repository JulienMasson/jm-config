#!/bin/bash

# set jm-config awesome config
AWESOME_HOME="~/.config/awesome"
[ ! -d $AWESOME_HOME ] && mkdir -p $AWESOME_HOME
cp ~/jm-config/awesome/rc.lua ~/.config/awesome/
cp -r ~/jm-config/awesome/debian ~/.config/awesome/

# get blingbling
cd ~/.config/awesome/
git clone -b v1.0 https://github.com/cedlemo/blingbling.git

# manual tiling
git clone https://github.com/zarkone/hand-tiler.git
