#!/bin/bash

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)
mydir=$(dirname $myself)

# check arguments
if [ $# -ne 1 ]; then
    echo "Usage: $myfile service"
    exit 1
fi

service=$1
cmd=$2

for i in 1 2 3; do
    host=manager-$i
    echo $host
    eval $(docker-machine env $host)
    did=$(docker ps | grep $service | cut -d" " -f1) 
    if [ -n "$did" ]; then
        docker logs -f $did
	exit 0
    fi
done

echo "container not found"
exit 1
