docker-compose build --pull theremine
img=$(pwd | rev | cut -d"/" -f1 | rev | tr '[:upper:]' '[:lower:]')_theremine
docker tag $img registry.consomalin.ovh:443/theremine
docker push registry.consomalin.ovh:443/theremine
