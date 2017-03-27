FROM node:7.4

WORKDIR /app

ADD package.json package.json
RUN npm install

RUN node_modules/eslint/bin/eslint.js .

ENV SERVER_PORT=80 \
    MONGO_URI=mongodb://mongo \
    KIVA_HOST=http://kiva \
    METRONOME_HOST=http://metronome \
    RABBIT_URI=amqp://rabbitmq:5672
    
ADD . .
CMD npm run build && \
    npm run start
