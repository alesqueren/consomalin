FROM haskell:8

RUN apt-get update && \
    apt-get install -y nginx netcat

RUN echo 'echo "$@" | xargs -n 1 | \
  sed -r -e "s/^([^\/]*)$/http:\/\/\\1\//" \
         -e "s/[^\/]$/\\0\//" \
         -e "s/\/\/([^:/]*)\//\/\/\\1:80\//" \
         -e "s/^(.*)\/\/(.*)@(.*):([^\/]*)\/(.*)$/\\3 \\4/" \
         -e "s/^(.*)\/\/(.*):([^\/]*)\/(.*)$/\\2 \\3/" | \
  xargs -n 2 nc -z' >> /bin/check && chmod +x /bin/check

WORKDIR app
ADD ./bin/wendy wendy
ADD ./resources ./resources

ENV SERVER_PORT=80 \
    MONGO_HOST=mongo \
    AMQP_HOST=rabbitmq \
    AMQP_USER=guest \
    AMQP_PASS=guest

HEALTHCHECK \
  CMD check $AMQP_HOST:5672 $MONGO_HOST:27017

CMD ./wendy
