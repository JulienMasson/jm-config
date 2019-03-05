#!/bin/bash

USERNAME="$1"
HOSTNAME="$2"
FULLNAME="$3"

# update packages
apt-get update
apt-get upgrade -y

# remove useless packages
apt-get purge -y vim-common vim-tiny

# install minimal packages
apt-get install -y locales sudo

# packages clean-up
apt-get -y autoremove
apt-get -y autoclean

# languages
sed -i "s/^# en_US/en_US/" /etc/locale.gen
locale-gen
update-locale LANG=en_US.UTF-8

# hostname
echo -n "${HOSTNAME}" > /etc/hostname

# create user and add him to couple groups
adduser "${USERNAME}" --gecos "${FULLNAME},,," --disabled-password
adduser "${USERNAME}" sudo

# remove all files in user directory
rm -rf "/home/${USERNAME}/*"
rm -rf "/home/${USERNAME}/.*"

# no password when sudo
sed -i 's/^\(%sudo.*\)ALL$/\1NOPASSWD: ALL/g' /etc/sudoers
