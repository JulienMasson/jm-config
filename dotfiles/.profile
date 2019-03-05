# source bashrc
source "$HOME/.bashrc"

# set default PATH
PATH="/usr/bin:/usr/local/bin:/bin:/sbin"

# set env with ~/.local
PATH="$HOME/.local/bin:$PATH"
LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"
PKG_CONFIG_PATH="$HOME/.local/pkgconfig:$PKG_CONFIG_PATH"

# start graphic env
# if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then exec startx; fi
