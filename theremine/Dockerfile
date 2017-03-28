FROM node:7.4

RUN apt-get update && \
    apt-get install -y nginx

WORKDIR /app

ADD package.json package.json
RUN npm install

ADD . .
RUN npm run build

CMD nginx -c /app/docker/nginx.conf -g "daemon off;"
