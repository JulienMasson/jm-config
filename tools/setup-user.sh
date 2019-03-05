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
sudo apt-get install manpages-dev manpages-posix-dev

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
sudo apt-get install -y build-essential libncurses-dev flex bison libelf-dev libssl-dev bc
git clone git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git
pushd linux
make defconfig
make -j$(nproc)
popd
popd

# install emacs
pushd "${JM_SRC}"
git clone https://github.com/emacs-mirror/emacs.git
popd

# install ripgrep
pushd "${JM_SRC}"
git clone https://github.com/BurntSushi/ripgrep.git
popd

# install google chrome
pushd Downloads
sudo apt-get install -y wget
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb
popd

# packages clean-up
sudo apt-get -y autoremove
sudo apt-get -y autoclean
