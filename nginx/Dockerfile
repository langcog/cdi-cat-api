FROM nginx:alpine

COPY nginx.conf /etc/nginx/nginx.conf
RUN rm /etc/nginx/conf.d/default.conf
# RUN mkdir /etc/nginx/conf.d/sites-enabled
# COPY apps/*.conf /etc/nginx/conf.d/

RUN apk update && apk add certbot certbot-nginx
