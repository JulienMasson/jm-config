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
#
# TIPS AND TRICKS:
# - autologin on systemd
# change in /lib/systemd/system/getty@.service
# ExecStart=-/sbin/agetty --noclear -a julien %I $TERM
#
# - install realtek wifi binairies
# $ sudo apt-get install firmware-realtek -y
#
# - connect to wireless
# $ ip link show wlan0
# $ sudo ip link set wlan0 up
# $ sudo iwlist scan
# $ wpa_passphrase SFR_9BB8 > wpa_supplicant.conf
# $ wpa_supplicant -B -D wext -i wlan0 -c wpa_supplicant.conf
# $ iwconfig && ifconfig
# $ sudo dhclient wlan0
#
# - run sudo command with no password:
# $ sudo visudo
# %sudo   ALL=(ALL:ALL) NOPASSWD: ALL
#
# - Reduce timeout of grub: /etc/default/grub
# GRUB_TIMEOUT=0
# $ sudo update-grub
#
# - update all git submodules on master
# $ git submodule foreach git checkout master
# $ git submodule foreach git pull --rebase

# basic setup
cd ~
mkdir bin Documents Downloads Pictures Music Desktop .config

# basic packages
sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get dist-upgrade -y
sudo apt-get install xinit rxvt-unicode arandr cscope offlineimap alsa-utils pulseaudio rofi i3lock-fancy dunst dbus-x11 tint2 thunar evince mairix eog install-info locate -y

# OPTIONAL packages
sudo apt-get install aptitude pv lxappearance network-manager-gnome numix-gtk-theme numix-icon-theme gnome-system-monitor -y

# packages for emacs
sudo apt-get install build-essential autogen autoconf automake libtool texinfo html2text libc6-dev libncurses5-dev libpng-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libx11-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxpm-dev libxrandr-dev libxt-dev libxtst-dev libxv-dev libgif-dev libtiff5-dev libgtk-3-dev libncurses5-dev libgtk2.0-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libtiff5-dev libxml2-dev librsvg2-dev libotf-dev libm17n-dev libgpm-dev libgconf2-dev libdbus-1-dev libgmime-2.6-dev libxapian-dev gnutls-dev libmagickcore-dev libmagick++-dev -y

# packages for jwm
sudo apt-get install libx11-xcb-dev libxcb-ewmh-dev libxcb-icccm4-dev libxcb-randr0-dev libxcb-keysyms1-dev

# install firefox
pushd Downloads
wget https://ftp.mozilla.org/pub/firefox/releases/57.0.1/linux-x86_64/en-US/firefox-57.0.1.tar.bz2
tar xvf firefox-57.0.1.tar.bz2
popd
ln -s ~/Downloads/firefox/firefox bin/firefox

# git clone repo in ~/bin/
pushd bin
git clone https://github.com/JulienMasson/jwm.git jwm-repo
git clone https://github.com/emacs-mirror/emacs.git emacs-repo
popd

# compile/setup awesome
pushd bin/jwm-repo
make -j
popd
ln -s ~/bin/jwm-repo/jwm bin/jwm

# compile/setup emacs
pushd bin/emacs-repo
./configure
make bootstrap -j
popd
ln -s ~/bin/emacs-repo/src/emacs bin/emacs

# compile emacs modules
pushd jm-config/emacs/modules/bbdb
./autogen.sh
./configure
make
popd
pushd jm-config/emacs/modules/magit
make
popd

# symlink all dotfiles in home directory
find ~/jm-config/dotfiles/ -name ".*" -exec sh -c 'ln -sf {} ~/$(basename {})' \;
