#!/bin/bash

myself=$(readlink -f ${BASH_SOURCE[0]})
mydir=$(dirname $myself)

#echo $(shuf -n 1 $mydir/programmers.txt)-$(cat /dev/urandom | tr -dc 'a-f0-9' | fold -w 4 | head -n 1)
echo worker-$(shuf -n 1 $mydir/programmers.txt)
