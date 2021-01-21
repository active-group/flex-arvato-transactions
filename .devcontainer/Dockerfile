# Build stage 0
FROM erlang:22-alpine

# install git and build-tools for lager backend
RUN apk add --update git
RUN apk add --update gcc
RUN apk add --update g++
RUN apk add libc-dev
RUN apk add bash

# Expose relevant ports
EXPOSE 8000
EXPOSE 8443
EXPOSE 4369
