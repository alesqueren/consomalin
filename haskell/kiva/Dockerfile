FROM haskell:8

RUN stack setup

RUN apt-get update && \
    apt-get install -y git libcurl4-openssl-dev

WORKDIR app
ADD . .

RUN stack install drive:kiva

ENV SERVER_PORT=80 \
    MONGO_HOST=mongo

CMD kiva
