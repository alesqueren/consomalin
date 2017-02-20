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
    echo "Usage: $myfile"
    exit 1
fi

ex "docker-compose build --pull theremine"

img=$(pwd | rev | cut -d"/" -f1 | rev | tr '[:upper:]' '[:lower:]')_theremine
dst_img="registry.consomalin.ovh:443/$service:$GO_PIPELINE_LABEL"
ex "docker tag $img $dst_img"
ex "docker push $dst_img"
