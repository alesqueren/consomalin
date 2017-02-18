FROM haskell:8

RUN stack setup

RUN apt-get update && \
    apt-get install -y git libcurl4-openssl-dev

RUN stack install hlint

WORKDIR app
ADD . .

RUN hlint .
RUN stack install drive:byte-diver

ENV MONGO_HOST=mongo

#CMD byte-diver
CMD sleep infinity
