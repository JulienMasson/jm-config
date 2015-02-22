#!/bin/bash

# move jm-config folder
cd ~/jm-config/

# pull all git submodules
git submodule update --init --recursive

# compile all emacs modules
cd emacs
emacs -batch -f batch-byte-compile *.el
emacs --batch --eval '(byte-recompile-directory "~/jm-config/emacs")'

# copy .emacs
cp ~/jm-config/emacs/.emacs ~/
