#!/bin/bash
set -e

docker-compose build --pull kiva
img=$(pwd | rev | cut -d"/" -f1 | rev | tr '[:upper:]' '[:lower:]')_kiva
docker tag $img registry.consomalin.ovh:443/kiva
docker push registry.consomalin.ovh:443/kiva
