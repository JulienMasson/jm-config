#!/bin/bash

span_hi='<span color="#ff8700">'   #colour for normal battery
span_lo='<span color="#aaaaaa">'     #colour for normal battery
span_sep='<span color="#ff8700">'  #colour for normal battery
span_warn='<span color="#ff0000">'

# Conky
read music mails uploadspeed downloadspeed free ip ram cpu_perc cpu_degree batt volume others <<<$(conky -c ~/jm-config/others/.conkyrc-status | tr -d ' ')

## Create the output
echo ' ' \
' Music   '\
$span_sep$music'</span>' \
'  |  Mail   '\
$span_sep$mails'</span>' \
'  |  ▲   '\
$span_sep$downloadspeed'</span>' \
'  |  ▼   '\
$span_sep$downloadspeed'</span>' \
'  |  Home   '\
$span_sep$free'</span>' \
'  |  IP   '\
$span_sep$ip'</span>' \
'  |  Ram   '\
$span_sep$ram'</span>' \
'  |  Cpu   '\
$span_sep$cpu_perc'%  -  '$cpu_degree'</span>' \
'  |  Battery   '\
$span_sep$batt'%''</span>' \
'  |  Volume   '\
$span_sep$volume'%''</span>' \
'  |  ' \
$span_lo`date "+%a, %d.%m.%Y."`'  -  </span>' \
$span_hi`date +"%R"`' </span> '
