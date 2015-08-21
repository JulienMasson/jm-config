#!/bin/bash

# get all ip from Intel Corporate Computer
INTEL_CORPORATE=$(sudo arp-scan --interface=eth0 --localnet | grep "Intel Corporate")

# filter the results to get only IP
INTEL_CORPORATE=$( echo $INTEL_CORPORATE | grep -o '[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}')

# nmap on each address
IP_FOUND=''
RESULT=""
while read -r line; do
    RESULT=$(nmap $line | grep "tllabx2")
    if [ ! -z "$RESULT" ]; then
    	IP_FOUND=$line
    	break
    fi
done <<< "$INTEL_CORPORATE"

# print result
if [ -n $IP_FOUND ]
then
    echo "IP tllabx2: " $IP_FOUND
else
    echo "IP NOT FOUND"
fi


