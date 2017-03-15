FROM node:7.4

WORKDIR /app
ADD . .

RUN npm install

ENV SERVER_PORT=80 \
    MONGO_URI=mongodb://mongo \
    KIVA_HOST=http://kiva \
    METRONOME_HOST=http://metronome \
    RABBIT_URI=amqp://rabbitmq:5672
    
CMD npm start
