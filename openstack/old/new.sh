#!/bin/bash
#. ./openrc.sh
#nova keypair-add --pub-key ~/.ssh/id_rsa.pub supervisor
set -e

myself=$(readlink -f ${BASH_SOURCE[0]})
myfile=$(basename $myself)
mydir=$(dirname $myself)

echo "*** Creating machine"
srv_name=$(./name.sh)
nova boot \
    --key-name supervisor \
    --flavor vps-ssd-1 \
    --image "Fedora 25" \
    --nic net-id=8d3e91fd-c533-418f-8678-4252de201489 \
    $srv_name > /dev/null
    #--user-data $mydir/machines/fedora.sh \


echo "*** Waiting for $srv_name to be ready"
sleep 6

retries=0
max_retries=30
while [ -z "$srv_ip" ];
do
    sleep 5
    if [[ $retries == $max_retries ]]; then
	echo "Timeout"
	exit 1
    fi
    retries=$((retries+1))

    echo "waiting for ip..."
    srv_ip=$(nova list | grep "| "$srv_name | cut -d'|' -f7 | cut -d= -f2 | sed s/\ //g)
done

echo "ip= $srv_ip"

echo "waiting for ssh..."
while ! nc -z $srv_ip 22;
do
    sleep 0.5
done


echo "*** Install docker"
cat machines/fedora.sh | xargs -L 1 ssh -o StrictHostKeyChecking=no fedora@$srv_ip


#echo "*** Add to docker-machine"
#docker-machine create -d generic --generic-ip-address $srv_ip --generic-ssh-user fedora $srv_name
#
#
#echo "*** Join swarm"
#manager_name=$(docker-machine ls | grep manager | grep Running | head -n1 | cut -d" " -f1)
#eval $(docker-machine env $manager_name)
#join_cmd=$(docker swarm join-token worker | tail -n4 | sed 's/\\//g')
#echo $join_cmd
#
#eval $(docker-machine env $srv_name)
#eval $join_cmd







#ssh admin@167.114.246.140
#
#dm create -d generic --generic-ip-address 167.114.246.140 --generic-ssh-user admin manager1
#dm ssh manager1
#docker swarm init --advertise-addr 167.114.246.140
#docker swarm join-token manager
#
#dm ssh manager2
#...
#
#nova delete stallman
