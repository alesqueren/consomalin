FROM node:7.4

WORKDIR /app
ADD . .

RUN npm install

ENV SERVER_PORT=80 \
    MONGO_URI=mongodb://mongo
    
CMD npm start
