# -*- mode: dockerfile; coding: utf-8 -*-
FROM debian:bullseye

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt upgrade --yes && apt install --yes git make
RUN git clone https://github.com/scheme-live/live.git

RUN cd /live && make prepare-debian

RUN cd /live && ./venv $(pwd)/local/ scheme-live install /

RUN rm -rf /var/cache/apt/* /tmp
