#!/bin/bash

# set jm-config awesome config
AWESOME_HOME="~/.config/awesome"
[ ! -d $AWESOME_HOME ] && mkdir -p $AWESOME_HOME
cp ~/jm-config/awesome/rc.lua ~/.config/awesome/
cp -r ~/jm-config/awesome/debian ~/.config/awesome/

# get blingbling
cd $AWESOME_HOME
git clone -b v1.0 git://github.com/cedlemo/blingbling.git
