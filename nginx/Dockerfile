FROM debian

EXPOSE 80 443

# get packages and sources for nginx
RUN apt-get update && \
    apt-get install -y build-essential zlib1g-dev libpcre3 libpcre3-dev libbz2-dev libssl-dev tar unzip libldap2-dev git gcc gettext netcat && \
    git clone https://github.com/nginx/nginx.git && \
    git clone https://github.com/kvspb/nginx-auth-ldap.git

# install nginx
WORKDIR /nginx
RUN ./auto/configure \
      --sbin-path=/usr/local/sbin \
      --add-module=/nginx-auth-ldap \
      --with-http_ssl_module \
      --with-http_gunzip_module \
      --with-http_gzip_static_module \
    && \
    make install 

# install dnsmasq (for /etc/hosts resolution)
RUN apt-get install -y dnsmasq && \
    echo "\n\n# Docker extra config \nuser=root\naddn-hosts=/etc/hosts\n" >> /etc/dnsmasq.conf

WORKDIR /app
ADD . .
RUN mv healthcheck /bin

HEALTHCHECK \
  CMD healthcheck /app/conf

CMD service dnsmasq restart && \
    ./mkvhosts.sh && \
    nginx -c /app/conf/nginx.conf -g "daemon off;"
