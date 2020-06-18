#!/bin/bash
set -e

if [ -z $OS_PASSWORD ]; then
    . ./openrc.sh
    #nova keypair-add --pub-key ~/.ssh/id_rsa.pub supervisor
fi

srv_name=$(./name.sh)

docker-machine create \
    -d openstack \
    --openstack-flavor-name="vps-ssd-1" \
    --openstack-image-name="UbuntuDocker" \
    --openstack-net-name="Ext-Net" \
    --openstack-ssh-user="ubuntu" \
    $srv_name
#--openstack-image-name="Ubuntu 14.04" \

eval $(docker-machine env $manager_name)
docker -v

manager_name=$(docker-machine ls | grep manager | grep Running | head -n1 | cut -d" " -f1)
eval $(docker-machine env $manager_name)
join_cmd=$(docker swarm join-token worker | tail -n4 | sed 's/\\//g')

eval $(docker-machine env $srv_name)
eval $join_cmd
