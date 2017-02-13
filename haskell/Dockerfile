FROM haskell:7

RUN stack setup

RUN apt-get update && \
    apt-get install -y libcurl4-openssl-dev

ADD . .

RUN stack install
