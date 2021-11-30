# -*- mode: dockerfile; coding: utf-8 -*-
FROM ubuntu:20.04

ADD . /live

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update && apt upgrade && apt install make && cd /live && make prepare-debian

RUN cd /live && rm -rf local/opt && ./venv scheme-live install

run rm -rf /var/cache/apt/* /tmp
RUN mkdir /tmp

CMD ["bash"]
