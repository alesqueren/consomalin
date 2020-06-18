#!/bin/bash
set -e

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)

cd /etc/letsencrypt/

if [ $# -ne 1 ]; then
    echo "Usage: $myfile domain"
    exit 1
fi
certbot certonly --webroot -w /webroot -d $1

mv nginx /tmp/letsencrypt
cp -rL /etc/letsencrypt/{live,nginx}
