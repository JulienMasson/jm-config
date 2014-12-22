#!/bin/bash

# emacs
sudo apt-get install emacs -y

# wl
sudo apt-get install wl -y

# cscope ctags
sudo apt-get install cscope exuberant-ctags -y

# zsh
sudo apt-get install zsh -y

# conky
sudo apt-get install conky -y

# dmenu
sudo apt-get install dmenu -y

# awesome
sudo apt-get install awesome awesome-extra -y

# pcmanfm
sudo apt-get install pcmanfm -y

# feh
sudo apt-get install feh -y

# numlockx
sudo apt-get install numlockx -y

# oocairo
cd ~/jm-config/awesome/modules/
sudo dpkg -i liblua5.1-oocairo0_1.4-1.2_amd64.deb
sudo dpkg -i liblua5.1-oocairo-dev_1.4-1.2_amd64.deb
