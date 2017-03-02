FROM haskell:8

WORKDIR app
ADD ./bin/wendy wendy

ENV MONGO_HOST=mongo \
    AMQP_HOST=rabbitmq \
    AMQP_USER=guest \
    AMQP_PASS=guest

CMD ./wendy
