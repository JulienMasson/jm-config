#!/bin/bash

red='\e[0;31m'
green='\e[0;32m'
NC='\e[0m'
cd $1;
result=$(git remote show origin | tail -1 | sed -n 's/^.*(//p' | sed -n 's/).*//p')

if [[ $result =~ ^l ]]
then result=$(echo "$result" | sed -n 's/^.*local //p')
fi

if [[ $result =~ ^o ]]
then echo -e "$result"
#then echo -e "${red}$result"
fi

if [[ $result =~ ^u ]]
then echo -e "$result"
#then echo -e "${green}$result"
fi
