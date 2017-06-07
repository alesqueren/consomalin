Consomalin 
==========

A simple drive website

## Prequisites
* install git
* [install docker](https://docs.docker.com/engine/installation/)
* [install docker-compose](https://docs.docker.com/compose/install/)

## Install
```bash
git clone ssh://git@git.consomalin.ovh:2222/drive/drive.git
git clone ssh://git@git.consomalin.ovh:2222/drive/state.git drive/docker/state

cd drive/
cp docker/*.yml .
docker-compose -f compiler.yml up
docker-compose build
docker-compose up -d
```

## Update
```bash
docker-compose -f compiler.yml up
docker-compose build --pull
docker-compose kill
docker-compose rm -f
docker-compose up -d
```
