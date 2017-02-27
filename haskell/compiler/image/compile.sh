#!/bin/bash
set -e

function ex () {
    COLOR='\033[0;35m'
    NC='\033[0m'
    echo ""
    echo -e "${COLOR}[compiler]$ "$1"${NC}"
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

# setup
ex "stack setup --allow-different-user"
ex "stack install hlint --allow-different-user"

cd /app

ex "hlint ."

mkdir -p /app/bin
PATH=$PATH:/app/bin # remove stack install warning
ex "stack install --allow-different-user --local-bin-path /app/bin drive:$service"
