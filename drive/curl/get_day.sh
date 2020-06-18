day_id=$1

curl 'https://www.auchandrive.fr/drive/choixslot.retraitslotgrid.changejour/'$day_id \
    -v \
    --location \
    -b cookies.txt \
    -H 'Origin: https://www.auchandrive.fr' \
    -H 'Accept-Encoding: gzip, deflate, br' \
    -H 'Accept-Language: fr-FR,fr;q=0.8,en-US;q=0.6,en;q=0.4' \
    -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36' \
    -H 'Content-Type: application/x-www-form-urlencoded; charset=UTF-8' \
    -H 'Accept: */*' \
    -H 'Referer: https://www.auchandrive.fr/drive/choixslot' \
    -H 'X-Requested-With: XMLHttpRequest' \
    -H 'Connection: keep-alive' \
    --data 't%3Azoneid=forceAjax' --compressed
