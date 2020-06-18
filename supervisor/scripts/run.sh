#!/bin/bash
set -e

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)
mydir=$(dirname $myself)

function disp () {
    COLOR='\033[0;35m'
    NC='\033[0m'
    echo ""
    echo -e "${COLOR}[run@supervisor]$ "$@"${NC}"
}

function ex () {
    disp $@
    eval $@
}

# check arguments
if [ $# -ne 1 ]; then
    echo "Usage: $myfile version"
    exit 1
fi
version=$1

disp "connecting to swarm"
swarm_machine=$(docker-machine ls | head -n 2 | tail -n 1 | cut -d" " -f1)
eval $(docker-machine env $swarm_machine)

cd $mydir/../release
ex "docker stack deploy --with-registry-auth -c docker-compose/services_$version.yml drive"
sleep 10
ex "docker service update --force --update-parallelism 1 --update-delay 60s drive_nginx"
