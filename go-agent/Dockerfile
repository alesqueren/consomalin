FROM gocd/gocd-agent-deprecated:16.9.0

# configure git
RUN git config --global user.email "go@example.com" && \
    git config --global user.name "go-agent"

# install docker
RUN apt-get update && \
    apt-get -y install wget gettext-base && \
    wget -qO- https://get.docker.com/ | sh

# install docker-compose
RUN curl -L "https://github.com/docker/compose/releases/download/1.11.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose && \
    chmod +x /usr/local/bin/docker-compose

RUN mkdir -p /usr/share/go-agent/config/

WORKDIR /usr/share/go-agent/config/

CMD echo "agent.auto.register.key=$AGENT_KEY" > autoregister.properties && \
    bash -c 'PRODUCTION_MODE=N /usr/share/go-agent/agent.sh'
