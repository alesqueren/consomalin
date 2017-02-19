FROM haskell:8

RUN stack setup

RUN apt-get update && \
    apt-get install -y git libcurl4-openssl-dev

RUN stack install hlint

WORKDIR app
ADD . .
#ADD ./.stack-lib/ /root/.stack/snapshots/

#RUN hlint .
#RUN rm -rf ~/.stack/snapshots/ && mv .stack-lib ~/.stack/snapshots/
#RUN ls -la ~/.stack
#RUN stack install drive:kiva
#
#ENV SERVER_PORT=80 \
#    MONGO_HOST=mongo
#
#CMD kiva
