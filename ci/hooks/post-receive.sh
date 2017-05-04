#!/bin/bash

read oldrev newrev refname
null="0000000000000000000000000000000000000000" 
#echo $oldrev
#echo $newrev
#echo $refname

if [[ "$oldrev" == "$null" || "$newrev" == "$null" ]]; then
    echo "tag push, exit"
    exit 0
fi

echo "Trigger Drive pipeline on gocd:"
n=0
res=1
while [[ "$res" -ne 0 && "$n" -ne "10" ]]; 
do
    curl --fail 'https://ci.consomalin.ovh/go/api/pipelines/Drive/schedule' \
        -u 'prod:hB3i944IrT' \
        -H 'Confirm: true' \
        -X POST \
        --insecure 2> /dev/null
    res=$?
    n=`expr $n + 1`
    if [[ "$res" -ne 0 ]]; then
        sleep 2
    fi  
done
