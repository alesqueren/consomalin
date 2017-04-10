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
if [ $# -ne 0 ]; then
  if [ $# -gt 2 ]; then
    echo "Usage: $myfile service [haskell]"
    exit 1
  fi
fi
service=$1
haskell=$2

ex "cp docker/docker-compose.yml docker/commands.yml ."
if [ -n "$haskell" ]; then
  ex "docker-compose -f haskell/compiler/docker-compose-srv.yml build --pull"
  ex "docker-compose -f haskell/compiler/docker-compose-srv.yml run compiler $service"
fi

filename=$(grep -l "  $service:" *.yml | head -n 1)
ex "docker-compose -f $filename build --pull $service"

img=$(pwd | rev | cut -d"/" -f1 | rev | tr '[:upper:]' '[:lower:]')_$service

dst_img="registry.consomalin.ovh:443/$service:$GO_PIPELINE_LABEL"
ex "docker tag $img $dst_img"
ex "docker push $dst_img"

dst_img="registry.consomalin.ovh:443/$service:latest"
ex "docker tag $img $dst_img"
ex "docker push $dst_img"
