#!/bin/bash

set -e

file="$(find dist/static/js -name app.*.js | head)"

echo "[RUN] substitue environment variables in $file"
envsubst '$DEMO $ANALYTICS_ID' < $file > $file.tmp
mv $file.tmp $file

echo "[RUN] start nginx"
nginx -c /app/docker/nginx.conf -g "daemon off;"
