#!/bin/bash

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

for i in 1 2 3; do
    host=manager-$i
    echo "searching on $host..."
    eval $(docker-machine env $host)
    did=$(docker ps | grep $service | head -n 1 | cut -d" " -f1) 
    if [ -n "$did" ]; then
	echo "container found, execute \"$cmd\":"
        docker exec -it $did $cmd
	exit 0
    fi
done

echo "no container found"
exit 1
