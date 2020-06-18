#!/bin/bash
set -e

function disp () {
    COLOR='\033[0;35m'
    NC='\033[0m'
    echo ""
    echo -e "${COLOR}[release@supervisor]$ "$@"${NC}"
}

# setup:
#   sudo cpan YAML
#   sudo cpan Hash::Merge::Simple

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)
mydir=$(dirname $myself)

# check arguments
if [ $# -gt 0 ]; then
    echo "Usage: $myfile"
    exit 1
fi

# disp "make docker-compose.yml"
# cd $mydir/../release
# perl -MYAML=LoadFile,Dump -MHash::Merge::Simple=merge \
#   -E 'say Dump(merge(map{LoadFile($_)}@ARGV))' \
#   docker-compose/{services,bs,tools}.yml \
#   > docker-compose.yml
# sed -i 's/version:\ 3/version:\ "3"/' docker-compose.yml

cd $mydir/../release
disp "commit changes"
git add .

#service_commit=$(grep image: docker-compose/services.yml | head -n 1 | cut -d: -f4)
#tools=$(md5sum docker-compose/tools.yml | cut -c1-7)
#bs=$(md5sum docker-compose/bs.yml | cut -c1-7)
#env=$(cat env/* | md5sum | cut -c1-7)
#commit_msg="services:$service_commit, tools:$tools, bs:$bs, env:$env"
commit_msg="commited by release.sh"

user=$(logname)
if [[ -z $user ]]; then
    user="admin"
fi

git commit -am "$commit_msg" --author "$user <>" || exit 3
