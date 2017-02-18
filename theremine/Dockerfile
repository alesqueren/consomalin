FROM node:7.4

RUN npm i -g eslint eslint-plugin-import eslint-config-airbnb-base

WORKDIR /app
ADD . .

RUN eslint .

RUN npm install

ENV SERVER_PORT=80 \
    MONGO_URI=mongodb://mongo
    
CMD npm start
