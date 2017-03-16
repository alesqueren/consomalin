FROM node:7.4

WORKDIR app
ADD ./bin/metronome metronome

ENV SERVER_PORT=80 \
    MONGO_HOST=mongo \
    EXPIRATION=300 \
    DRIVE_USER=user \
    DRIVE_PASS=pass

CMD ./metronome
