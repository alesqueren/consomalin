FROM haskell:8

WORKDIR app
ADD ./bin/metronome metronome

ENV EXPIRATION=300 \
    DRIVE_USER=user \
    DRIVE_PASS=pass

CMD ./metronome
