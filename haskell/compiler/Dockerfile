FROM haskell:8

RUN apt-get update && \
    apt-get install -y git libcurl4-openssl-dev

WORKDIR /app
ADD compile.sh .

CMD compile.sh
