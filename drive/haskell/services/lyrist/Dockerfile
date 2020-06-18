FROM haskell:8

WORKDIR app
ADD ./bin/lyrist lyrist

ENV SMTP_HOST=mail.consomalin.ovh \
    SMTP_USER= \
    SMTP_PASS= \
    IMAP_HOST=mail.consomalin.ovh \
    IMAP_USER= \
    IMAP_PASS= \
    AMQP_HOST=rabbitmq \
    MONGO_HOST=mongo

CMD ./lyrist
