#!/bin/bash
set -e

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)
mydir=$(dirname $myself)

function disp () {
    COLOR='\033[0;35m'
    NC='\033[0m'
    echo ""
    echo -e "${COLOR}[build_release_run@supervisor]$ "$@"${NC}"
}

function ex () {
    disp $@
    eval $@
}

# check arguments
if [ $# -gt 3 ]; then
    echo "Usage: $myfile [version] [commit]"
    exit 1
fi
version=$1
if [ -z "$version" ]; then
    version="master"
fi
commit=$2

set +e
ex "$mydir/build.sh $version $commit" 
if [[ "$?" != 0 && "$?" != 3 ]]; then
    exit 1
fi

ex "$mydir/release.sh"
if [[ "$?" != 0 && "$?" != 3 ]]; then
    exit 1
fi
set -e

ex "$mydir/run.sh $version" 
