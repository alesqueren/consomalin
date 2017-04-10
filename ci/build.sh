#!/bin/bash
set -e

# export GO_PIPELINE_LABEL=666
# export HASKELL_PATH=./haskell 
# export STACK_PATH=./haskell/.stack-compiler 

function ex () {
    COLOR='\033[0;35m'
    NC='\033[0m'
    echo ""
    echo -e "${COLOR}[ci]$ "$1"${NC}"
    eval $1
}

function disp () {
    COLOR='\033[0;35m'
    NC='\033[0m'
    echo ""
    echo -e "${COLOR}[ci]$ "$1"${NC}"
}

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)

# check arguments
if [ $# -ne 0 ]; then
    echo "Usage: $myfile"
    exit 1
fi

# check env variables
if [[ -z "$GO_PIPELINE_LABEL" || -z "$HASKELL_PATH" || -z "$STACK_PATH" ]]; 
then
  echo "Env variables not correctly set"
  exit 1
fi

ex "cp docker/docker-compose.yml docker/commands.yml."
ex "envsubst < ci/compiler.yml > compiler.yml"

ex "docker-compose -f compiler.yml build --pull"
ex "docker-compose -f compiler.yml run compiler"

ex "docker-compose -p $GO_PIPELINE_LABEL -f docker-compose.yml build --pull"
ex "docker-compose -p $GO_PIPELINE_LABEL -f commands.yml       build --pull"

disp "get services..."
services=$(grep build -B1 docker-compose.yml commands.yml | \
  grep -v build | grep : \
  | cut -d" " -f3 | cut -d: -f1)
disp "$services"

for service in $services;
do
  local_img=$GO_PIPELINE_LABEL"_"$service
  distant_img="registry.consomalin.ovh:443/$service:$GO_PIPELINE_LABEL"

  ex "docker tag $local_img $distant_img"
  ex "docker push $distant_img"
done
