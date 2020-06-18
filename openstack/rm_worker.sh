#!/bin/bash
set -e 

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)
mydir=$(dirname $myself)

function usage() {
    echo "Usage: $myfile [node]"
    exit 1
}

if [ $# -gt 1 ]; then
    usage
fi
if [ $# -eq 1 ]; then
    worker_name=$1
else
    worker_name=$(docker-machine ls | grep worker | head -n1 | cut -d" " -f1)
    if [ -z "$worker_name" ]; then
        echo "No worker to suppress."
	exit 1
    fi
fi

if [ -z $OS_PASSWORD ]; then
    . ./openrc.sh
fi

manager_name=$(docker-machine ls | grep manager | grep Running | head -n1 | cut -d" " -f1)
eval $(docker-machine env $manager_name)
docker node update --availability drain $worker_name

docker-machine rm -f $worker_name
nova delete $worker_name
