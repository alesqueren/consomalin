FROM node

EXPOSE 8081

RUN npm install -g mongo-express

WORKDIR /usr/local/lib/node_modules/mongo-express/
ADD config.js config.js

ENV ME_CONFIG_MONGODB_SERVER="mongo" \
    ME_CONFIG_USE_BASICAUTH="false" \
    ME_CONFIG_BASICAUTH_USERNAME="" \
    ME_CONFIG_BASICAUTH_PASSWORD=""

CMD node app.js
