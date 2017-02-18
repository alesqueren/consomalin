#!/bin/bash
set -e

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)

# check arguments
if [ $# -ne 1 ]; then
    echo "Usage: $myfile service"
    exit 1
fi
service=$1

docker-compose build --pull $service
img=$(pwd | rev | cut -d"/" -f1 | rev | tr '[:upper:]' '[:lower:]')_$service
docker tag $img registry.consomalin.ovh:443/$service
docker push registry.consomalin.ovh:443/$service
