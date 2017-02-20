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
cd drive
git clone ssh://git@git.consomalin.ovh:2222/drive/state.git

docker-compose -f haskell/compiler/docker-compose.yml up
docker-compose up -d
```

## Update
```bash
docker-compose -f haskell/compiler/docker-compose.yml up
docker-compose up -d --build
```
