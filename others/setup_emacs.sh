#!/bin/bash

# move emacs folder
cd ~/config/emacs/modules/

# pull all git submodules
git submodule update --init ac-c-headers
git submodule update --init ac-math
git submodule update --init ample-zen
git submodule update --init apt-utils-ido
git submodule update --init auctex
git submodule update --init auto-complete
git submodule update --init auto-complete-auctex
git submodule update --init browse-kill-ring
git submodule update --init c-eldoc
git submodule update --init dtrt-indent
git submodule update --init emacs-ac-etags
git submodule update --init emacs-bash-completion
git submodule update --init erc-nick-notify
git submodule update --init git-modes
git submodule update --init latex-extra
git submodule update --init magic-latex-buffer
git submodule update --init magit
git submodule update --init magit-filenotify
git submodule update --init popup-el
git submodule update --init predictive
git submodule update --init purple
git submodule update --init shell-history
git submodule update --init status
git submodule update --init virtual-desktops.el
git submodule update --init wanderlust
git submodule update --init w3m
git submodule update --init xcscope.el
git submodule update --init yasnippet
