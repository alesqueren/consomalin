FROM haskell:8

RUN stack setup

RUN apt-get update && \
    apt-get install -y git libcurl4-openssl-dev

RUN stack install hlint

WORKDIR app
ADD . .

RUN hlint .
RUN stack install drive:kiva

ENV SERVER_PORT=80 \
    MONGO_HOST=mongo

CMD kiva
