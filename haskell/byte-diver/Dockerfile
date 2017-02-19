FROM haskell:8

RUN stack setup && \
    stack upgrade
RUN stack install hlint

RUN apt-get update && \
    apt-get install -y git libcurl4-openssl-dev

WORKDIR app
ADD . .

RUN hlint .
RUN stack install drive:byte-diver

ENV MONGO_HOST=mongo

#CMD byte-diver
CMD sleep infinity
