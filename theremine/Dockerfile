FROM node:7.4

RUN apt-get update && \
    apt-get install -y nginx netcat gettext

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

HEALTHCHECK \
  CMD check localhost

ADD . .
RUN npm run lint && \
    npm run build

CMD docker/run.sh
