#!/bin/bash
set -e

export cred='prod:hB3i944IrT'
version=$1

for i in `seq 0 10 100`;
do
    echo $(curl "https://ci.consomalin.ovh/go/api/pipelines/$version/history/$i" -u $cred --insecure 2> /dev/null)
done
