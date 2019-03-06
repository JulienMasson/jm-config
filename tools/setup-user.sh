#!/bin/bash

cd ~
JM_CONFIG=".config/jm"
JM_SRC=".local/src"

# create directories
mkdir bin Documents Downloads Pictures Music Desktop .config .cache .local
mkdir .local/{bin,lib,pkgconfig,src,share}

# update packages
sudo apt-get update
sudo apt-get upgrade -y

# install git
sudo apt-get install -y git

# install manpages
sudo apt-get install -y manpages-dev manpages-posix-dev

# install compilation tools
sudo apt-get install -y build-essential autogen autoconf automake meson

# install programming tools
sudo apt-get install -y cscope ripgrep locate

# install terminal
sudo apt-get install -y rxvt-unicode

# install X utils
sudo apt-get install -y xinit arandr

# install pdf viewer
sudo apt-get install -y evince

# install image viewer
sudo apt-get install -y eog

# install mail sync
sudo apt-get install -y offlineimap

# install jm-config
read -s -p "Password (jm-config): " JM_PASSWORD
echo
git clone https://JulienMasson@github.com/JulienMasson/jm-config.git "${JM_CONFIG}"
pushd "${JM_CONFIG}"
sed -i "s/massonju@gitlab.com/massonju:${JM_PASSWORD}@gitlab.com/g" .gitmodules
git submodule sync
git submodule update --init --recursive
sed -i "s/massonju:${JM_PASSWORD}@gitlab.com/massonju@gitlab.com/g" .gitmodules
git submodule sync
popd

# install symbolic link
find "${JM_CONFIG}/dotfiles/" -name ".*" -exec sh -c 'ln -sf {} ~/$(basename {})' \;
find "${JM_CONFIG}/emacs/modules/jm-private/dotfiles/" -name ".*" -exec sh -c 'ln -sf {} ~/$(basename {})' \;

# compile kernel
pushd "${JM_SRC}"
sudo apt-get install -y libncurses-dev flex bison libelf-dev libssl-dev bc
git clone git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git
pushd linux
make defconfig
make -j$(nproc)
popd
popd

# install jwm
pushd "${JM_SRC}"
git clone https://github.com/JulienMasson/jwm.git jwm
sudo apt-get install -y check libx11-xcb-dev libxcb-randr0-dev libxcb-keysyms1-dev libxcb-ewmh-dev libxcb-icccm4-dev libxcb-util0-dev libcairo2-dev libpangocairo-1.0-0 libpango1.0-dev
pushd jwm
make
popd
popd

# install google chrome
pushd Downloads
sudo apt-get install -y wget fonts-liberation libappindicator3-1 libasound2 xdg-utils
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb
popd

# install emacs
pushd "${JM_SRC}"
git clone https://github.com/emacs-mirror/emacs.git
sudo apt-get install -y texinfo libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev
pushd emacs
./autogen.sh
make bootstrap -j$(nproc)
popd
popd

# packages clean-up
sudo apt-get -y autoremove
sudo apt-get -y autoclean
