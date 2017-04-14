FROM node:7.4

RUN apt-get update && \
    apt-get install -y netcat

RUN echo 'echo "$@" | xargs -n 1 | \
  sed -r -e "s/^([^\/]*)$/http:\/\/\\1\//" \
         -e "s/[^\/]$/\\0\//" \
         -e "s/\/\/([^:/]*)\//\/\/\\1:80\//" \
         -e "s/^(.*)\/\/(.*)@(.*):([^\/]*)\/(.*)$/\\3 \\4/" \
         -e "s/^(.*)\/\/(.*):([^\/]*)\/(.*)$/\\2 \\3/" | \
  xargs -n 2 nc -z' >> /bin/check && chmod +x /bin/check

WORKDIR /app

ADD package.json package.json
RUN npm install

ENV SERVER_PORT=80 \
    MONGO_URI=mongodb://mongo:27017/
    
HEALTHCHECK \
  CMD check localhost $MONGO_URI

ADD . .
RUN npm run build

CMD npm run start
