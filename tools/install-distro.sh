#!/bin/bash

# partitioning

# setup boot

# setup rootfs
ROOTFS="rootfs"
MIRROR="http://ftp.fr.debian.org/debian/"

mkdir -p "${ROOTFS}"

apt-get install debootstrap
debootstrap unstable "${ROOTFS}" "${MIRROR}"
echo -n "deb ${MIRROR} unstable main contrib non-free" > "${ROOTFS}/etc/apt/sources.list"

USERNAME="julien"
HOSTNAME="masson"
FULLNAME="Julien Masson"

cp setup-system.sh "${ROOTFS}"/root/
chroot "${ROOTFS}" /root/setup-system.sh "${USERNAME}" "${HOSTNAME}" "${FULLNAME}"
rm "${ROOTFS}"/root/setup-system.sh

cp setup-user.sh "${ROOTFS}/home/${USERNAME}"
chroot "${ROOTFS}" /bin/bash -c "su - ${USERNAME} -c /home/${USERNAME}/setup-user.sh"
rm "${ROOTFS}/home/${USERNAME}/setup-user.sh"
