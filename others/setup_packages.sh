#!/bin/bash

# packages for compiling emacs and modules
sudo apt-get install build-essential autogen autoconf automake libtool texinfo libc6-dev libncurses5-dev libpng-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libx11-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxpm-dev libxrandr-dev libxt-dev libxtst-dev libxv-dev libgif-dev libtiff5-dev libgtk-3-dev libncurses5-dev libgtk2.0-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libtiff4-dev libxml2-dev librsvg2-dev libotf-dev libm17n-dev libgpm-dev libgnutls-dev libgconf2-dev libdbus-1-dev libgmime-2.6-dev libxapian-dev -y

# cscope
sudo apt-get install cscope -y

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

# offlineimap
sudo apt-get install offlineimap -y

# lxappearance
sudo apt-get install lxappearance -y

# xscreensaver
sudo apt-get install xscreensaver -y

# oocairo
cd ~/jm-config/awesome/modules/
sudo dpkg -i liblua5.1-oocairo0_1.4-1.2_amd64.deb
sudo dpkg -i liblua5.1-oocairo-dev_1.4-1.2_amd64.deb
