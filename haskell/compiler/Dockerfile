FROM haskell:8

RUN apt-get update && \
    apt-get install -y git libcurl4-openssl-dev

WORKDIR .
ADD compile.sh .

ENTRYPOINT ["/compile.sh"]
