#!/bin/bash

# move jm-config folder
cd ~/jm-config/

# pull all git submodules
git submodule update --init --recursive
git submodule foreach git checkout master
git submodule foreach git pull --rebase

# compile submodule which have makefile
list=$(find emacs/modules/ -name Makefile)
for i in $list; do
    pushd $(dirname $i)
    make -j
    popd
done

# compile all emacs modules lisp code
emacs --batch --eval '(byte-recompile-directory "~/jm-config/emacs/" 0 t)'

# copy .emacs
cp ~/jm-config/emacs/.emacs ~/

# compile/clean emacs repo
# cd ~/bin/emacs/
# make distclean
# make all
