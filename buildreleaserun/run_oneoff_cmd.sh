#!/bin/bash
set -e

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)
mydir=$(dirname $myself)

function disp () {
    COLOR='\033[0;35m'
    NC='\033[0m'
    echo ""
    echo -e "${COLOR}[supervisor]$ "$@"${NC}"
}

function ex () {
    disp $@
    eval $@
}

# check arguments
if [ $# -lt 1 ]; then
    echo "Usage: $myfile service [args]"
    exit 1
fi
export service=$1

disp "connecting to swarm"
swarm_machine=$(docker-machine ls | head -n 2 | tail -n 1 | cut -d" " -f1)
eval $(docker-machine env $swarm_machine)

cd $mydir/../release
ex "docker-compose -f oneoff-cmd.yml pull $service"
ex "docker-compose -f oneoff-cmd.yml run $@"
