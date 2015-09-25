#!/bin/bash

# move jm-config folder
cd ~/jm-config/

# pull all git submodules
git submodule update --init --recursive
git submodule foreach git checkout master

# compile bbdb submodule
cd ~/jm-config/emacs/modules/bbdb/
./autogen.sh
./configure
make -j

# compile all emacs modules
cd emacs
emacs -batch -f batch-byte-compile *.el
emacs --batch --eval '(byte-recompile-directory "~/jm-config/emacs")'

# copy .emacs
cp ~/jm-config/emacs/.emacs ~/
