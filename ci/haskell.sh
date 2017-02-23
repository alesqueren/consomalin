#!/bin/bash
set -e

function ex () {
    COLOR='\033[0;35m'
    NC='\033[0m'
    echo ""
    echo -e "${COLOR}[ci]$ "$1"${NC}"
    eval $1
}

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)

# check arguments
if [ $# -ne 1 ]; then
    echo "Usage: $myfile service"
    exit 1
fi
service=$1

ex "docker-compose -f haskell/compiler/docker-compose-srv.yml build --pull"
ex "docker-compose -f haskell/compiler/docker-compose-srv.yml run compiler $service"
ex "docker-compose build --pull $service"

img=$(pwd | rev | cut -d"/" -f1 | rev | tr '[:upper:]' '[:lower:]')_$service

dst_img="registry.consomalin.ovh:443/$service:$GO_PIPELINE_LABEL"
ex "docker tag $img $dst_img"
ex "docker push $dst_img"

dst_img="registry.consomalin.ovh:443/$service:latest"
ex "docker tag $img $dst_img"
ex "docker push $dst_img"
