#!/bin/bash

# move jm-config folder
cd ~/jm-config/

# pull all git submodules
git submodule update --init --recursive
git submodule foreach git checkout master
git submodule foreach git pull --rebase

# compile bbdb submodule
cd ~/jm-config/emacs/modules/bbdb/
./autogen.sh
./configure
make -j

# compile all emacs modules lisp code
emacs --batch --eval '(byte-recompile-directory "~/jm-config/emacs/modules/" 0 t)'

# copy .emacs
cp ~/jm-config/emacs/.emacs ~/
