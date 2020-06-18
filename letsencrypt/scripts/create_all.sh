#!/bin/bash
set -e
domain=consomalin.ovh

cd /etc/letsencrypt/

rm -rf /tmp/letsencrypt
mkdir -p /tmp/letsencrypt
mv archive live renewal /tmp/letsencrypt || true

certbot certonly --webroot -w /webroot -d $domain
for s in amqp framapad share board git wiki ci phpldapadmin registry a b c;
do
    certbot certonly --webroot -w /webroot -d $s.$domain
done

mv nginx /tmp/letsencrypt
cp -rL /etc/letsencrypt/{live,nginx}

echo "done" 
