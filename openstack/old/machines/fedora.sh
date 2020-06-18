#!/bin/bash

#dnf update -y
#curl -sSL https://get.docker.com/ | sh
sudo curl -fsSL https://get.docker.com/ | sh

sudo groupadd docker || true
sudo usermod -aG docker fedora

sudo systemctl enable docker
sudo systemctl start docker
