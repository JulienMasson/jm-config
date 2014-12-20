#!/bin/bash

span_hi='<span color="#ff8700">'   #colour for normal battery
span_lo='<span color="#aaaaaa">'     #colour for normal battery
span_sep='<span color="#ff8700">'  #colour for normal battery
span_warn='<span color="#ff0000">'

## Set battery message (works on Thinkpad Edge 11 with tp_smapi driver)

# batt=`cat /sys/devices/platform/smapi/BAT0/remaining_percent`
# if (($batt <= 30));
# then battstate=$span_warn$batt'%% </span>'$span_lo`cat /sys/devices/platform/smapi/BAT0/state`'</span>'
# else battstate=$span_hi$batt'%% </span>'$span_lo`cat /sys/devices/platform/smapi/BAT0/state`'</span>'
# fi
batt="40%"
battstate="idle"

## Create the output

echo ' '$battstate$span_sep' | </span>'$span_lo`cat /proc/cpuinfo | grep "cpu MHz" | awk '{sum+=$4} END { print sum/NR,"MHz"}'`'</span>'$span_sep' | </span>'$span_lo`date "+%a, %d.%m.%Y."`' - </span>'$span_hi`date +"%R"`' </span>'
