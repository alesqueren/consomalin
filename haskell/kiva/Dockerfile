FROM haskell:8

WORKDIR app
ADD ./bin/kiva kiva

ENV SERVER_PORT=80 \
    MONGO_HOST=mongo

CMD ./kiva
