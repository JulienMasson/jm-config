#!/bin/bash

# go to jm-config scripts
cd ~/jm-config/others/

# setup packages
./setup_packages.sh

# setup emacs
./setup_emacs.sh

# copy .emacs
cp ~/jm-config/emacs/.emacs ~/
