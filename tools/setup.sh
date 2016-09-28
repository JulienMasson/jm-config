#!/bin/bash

# Setup when installing debian netinst image
#
# Prerequisites:
#
# - Add debian testing repo in /etc/apt/sources.list
# deb http://ftp.fr.debian.org/debian/ stretch main contrib non-free
# deb-src http://ftp.fr.debian.org/debian/ stretch main contrib non-free
#
# Optional: run dhclient (look network interface with "ip addr" command)
# $ /sbin/dhclient enp4s0
#
# - Install git
# $ sudo apt-get install git
#
# - Get jm-config
# $ cd ~ && git clone --recursive https://github.com/JulienMasson/jm-config.git


# basic setup
cd ~
mkdir bin Documents Downloads Pictures Music Desktop .config

# basic packages
sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get dist-upgrade -y
sudo apt-get install xinit rxvt-unicode arandr cscope slock numlockx offlineimap alsa-utils pulseaudio dmenu aptitude pcmanfm texlive iceweasel flashplugin-nonfree evince mairix eog install-info -y

# packages for emacs
sudo apt-get install build-essential autogen autoconf automake libtool texinfo libc6-dev libncurses5-dev libpng-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libx11-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxpm-dev libxrandr-dev libxt-dev libxtst-dev libxv-dev libgif-dev libtiff5-dev libgtk-3-dev libncurses5-dev libgtk2.0-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libtiff5-dev libxml2-dev librsvg2-dev libotf-dev libm17n-dev libgpm-dev libgconf2-dev libdbus-1-dev libgmime-2.6-dev libxapian-dev gnutls-dev -y

# packages for awesome
sudo apt-get build-dep awesome -y
sudo apt-get install libx11-xcb-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev lua-lgi libpango1.0-dev -y

# git clone repo in ~/bin/
pushd bin
git clone https://github.com/awesomeWM/awesome.git awesome-repo
git clone https://github.com/emacs-mirror/emacs.git emacs-repo
popd

# compile awesome
pushd bin/awesome-repo
make -j
cp awesome ../
popd

# setup awesome
ln -s jm-config/awesome/ .config/awesome
pushd jm-config/awesome/
cp .profile .Xauthority .xinitrc .xsessionrc .bashrc .Xdefaults ~
popd
# autologin on systemd
# change in /lib/systemd/system/getty@.service
# ExecStart=-/sbin/agetty --noclear -a julien %I $TERM

# compile emacs
pushd bin/emacs-repo
./configure
make -j
popd

# compile emacs modules
pushd jm-config/emacs/modules/bbdb
./configure
make
popd
pushd jm-config/emacs/modules/magit
make
popd

# setup emacs
ln -s bin/emacs-repo/src/emacs bin/emacs
cp jm-config/emacs/.emacs ~

# optional packages
sudo apt-get install pv htop lxappearance network-manager-gnome numix-gtk-theme gnome-system-monitor -y

# for realtek wifi chip
sudo apt-get install firmware-realtek -y