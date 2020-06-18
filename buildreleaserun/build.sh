#!/bin/bash
set -e

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)
mydir=$(dirname $myself)

function disp () {
    COLOR='\033[0;35m'
    NC='\033[0m'
    echo ""
    echo -e "${COLOR}[build@supervisor]$ "$@"${NC}"
}

# check arguments
if [ $# -gt 3 ]; then
    echo "Usage: $myfile version [commit]"
    exit 1
fi
if [ $# -lt 1 ]; then
    echo "Usage: $myfile version [commit]"
    exit 1
fi
export version=$1
export commit=$2

if [ -z "$commit" ]; then
    disp "fetching commits from gocd..."
    set +e
    n=0
    res=1
    while [[ "$res" -ne 0 ]]; 
    do
        if [[ "$n" -eq 10 ]]; then
            echo "error while fetching commits"
            exit 1
        fi  
        commits=$($mydir/lib/get_history.sh $version | python $mydir/lib/get_labels.py 2> /dev/null)
        res=$?
        n=`expr $n + 1`
        if [[ "$res" -ne 0 ]]; then
    	echo "..."
            sleep 2
        fi  
    done
    set -e

    commit=$(echo $commits | cut -d" " -f1)
    echo "use last valid commit: $commit"
fi

length=${#commit}
if [ "$length" != "7" ]; then
    echo "commit length must be 7"
    exit 1
fi


disp "making services yml ..."
cd $mydir/../release/
temp_file=/tmp/services.yml
template_file=templates/$version.yml
dest_file=docker-compose/services_$version.yml
touch $dest_file
envsubst < $template_file > $temp_file
if [ -z "$(diff $temp_file $dest_file)" ]; then
    rm $temp_file
    echo "$dest_file already use $commit, exit."
    exit 0
fi 
mv $temp_file $dest_file
