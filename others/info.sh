#!/bin/bash
case "$1" in

    volume)
	result=$(amixer get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "mute" } else { print $2"%" }}')
	;;

    battery)
	result="0%"
	;;

    cpu_degree)
	result="31°C"
	;;

    cpu_percentage)
	result="3%"
	;;

    ram)
	result=$(free | awk 'FNR == 3 {print $3}' | sed -r -e 's/^.{1}/&./' | cut -c1-4)
	;;

    ip)
	result=$(ifconfig | grep -A 1 'eth0' | tail -1 | cut -d ':' -f 2 | cut -d ' ' -f 1)
	;;

    home-data)
	result="434G"
	;;

    uploadspeed)
	result="34.7K"
	;;

    downloadspeed)
	result="0B"
	;;

    mail)
	result=$(ls -1 ~/Maildir/Gmail/INBOX/new/ | wc -l)
	;;

    music)
	result="off"
	;;
esac
echo -n "$result"
