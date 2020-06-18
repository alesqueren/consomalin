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

local_img="nginx_$GO_PIPELINE_LABEL"
dst_img="consomalin/nginx-ldap"

ex "docker build -t $local_img --pull ."
ex "docker tag $local_img $dst_img"
ex "docker push $dst_img"
