FROM haskell:8

RUN apt-get update && \
    apt-get install -y git libcurl4-openssl-dev

WORKDIR /bin
ADD compile.sh .

CMD compile.sh
