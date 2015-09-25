# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac


# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Colors:
black='\e[0;30m'
blue='\e[0;34m'
green='\e[0;32m'
cyan='\e[0;36m'
red='\e[0;31m'
purple='\e[0;35m'
brown='\e[0;33m'
lightgray='\e[0;37m'
darkgray='\e[1;30m'
lightblue='\e[1;34m'
lightgreen='\e[1;32m'
lightcyan='\e[1;36m'
lightred='\e[1;31m'
lightpurple='\e[1;35m'
yellow='\e[1;33m'
white='\e[1;37m'
nc='\e[0m'
NO_COLOUR="\[\033[0m\]"

# Functions and Scripts:
upinfo ()
{
echo -ne "\t ";uptime | awk /'up/ {print $3,$4,$5,$6,$7,$8,$9,$10}'
}

# 256 colors
function EXT_COLOR () { echo -ne "\[\033[38;5;$1m\]"; }

function last_two_dirs {
pwd |rev| awk -F / '{print $1,$2}' | rev | sed s_\ _/_
}

#------------------------------------------////
# Prompt:
#------------------------------------------////
PS1="${lightblue}\w:\n${lightblue}▶${NO_COLOUR} "

# System Information:
clear
# echo -ne "${red}Today is:\t\t${cyan}" `date`; echo ""
# echo -e "${red}Kernel Information: \t${cyan}" `uname -smr`
# echo -ne "${red}Uptime is: \t${cyan}";upinfo;echo ""

alias ll='ls -l'
alias reload='source ~/.bashrc'

# gitprompt configuration
GIT_PROMPT_ONLY_IN_REPO=1
# GIT_PROMPT_FETCH_REMOTE_STATUS=0   # uncomment to avoid fetching remote status
# GIT_PROMPT_START=...    # uncomment for custom prompt start sequence
# GIT_PROMPT_END=...      # uncomment for custom prompt end sequence
GIT_PROMPT_THEME=Custom # use custom .git-prompt-colors.sh
# GIT_PROMPT_THEME=Solarized # use theme optimized for solarized color scheme
source ~/jm-config/bash/bash-git-prompt/gitprompt.sh

alias switch-screens='xrandr --output DP-1 --auto && xrandr --output LVDS-1 --off && xrandr --output DP-3 --mode 1920x1200 --right-of DP-1 --output DP-1 --mode 1920x1200'
alias switch-labtop='xrandr --output DP-1 --off && xrandr --output LVDS-1 --mode 1920x1080 && xrandr --output DP-3 --off'
alias jm-update='sudo apt-get update && sudo apt-get upgrade -y && sudo apt-get dist-upgrade -y && sudo apt-get autoremove -y && sudo apt-get autoclean -y'
alias jm-install='sudo apt-get install'
alias jm-search='sudo apt-cache search'
alias jm-ps='ps aux | grep'
alias jm-env='env | grep'
