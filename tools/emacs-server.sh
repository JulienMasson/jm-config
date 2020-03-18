#!/bin/bash

function check_server {
    emacsclient -a false -e 't' &> /dev/null
    return $?
}

if ! check_server; then
    emacs --no-window-system --quick --daemon --load=/home/julienm/.config/jm/emacs/init.el
fi

while ! check_server; do sleep 1; done
emacsclient --tty
