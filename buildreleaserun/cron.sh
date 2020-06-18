#!/bin/bash

set -e

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)
mydir=$(dirname $myself)

# check arguments
if [ $# -ne 2 ]; then
    echo "Usage: $myfile service cmd"
    exit 1
fi

service=$1
cmd=$2
logfile=$mydir/../cron_logs/$service.logs

echo "" >> $logfile
date >> $logfile
$mydir/execit.sh $service $cmd &>> $logfile
