set -e

for l in $(env | grep ^VHOST_);
do
    export service=$(echo $l | cut -d= -f1 | cut -d_ -f2- | tr '[:upper:]' '[:lower:]')
    export domain=$(echo $l | cut -d= -f2)
    
    echo "creating vhost: $domain -> $service"
    envsubst '$service:$domain' < conf/vhost.template > conf/sites-enabled/$service
done
