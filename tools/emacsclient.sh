#!/bin/sh
emacsclient --server-file ~/emacs-server \
            /ssh:jmassonx@10.102.161.221:`readlink --canonicalize-missing $*`

