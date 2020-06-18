FROM haskell:8

RUN apt-get update && \
    apt-get install -y netcat

WORKDIR app
ADD ./bin/byte-diver byte-diver

ENV MONGO_HOST=mongo

CMD ./byte-diver
