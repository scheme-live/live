# -*- mode: dockerfile; coding: utf-8 -*-
FROM debian:bullseye-slim

ADD . /live

ENV DEBIAN_FRONTEND=noninteractive
ENV USER=star

RUN apt update
RUN apt upgrade
RUN apt install make
RUN cd /live && make prepare-debian
RUN rm -rf /var/lib/apt/lists/* && rm -rf /var/cache/apt/* -rf

# RUN cd /live && ./venv scheme-live chez install /usr/local
# RUN cd /live && ./venv scheme-live chez check

# RUN cd /live && ./venv scheme-live chibi install /usr/local
# RUN cd /live && ./venv scheme-live chibi check

# RUN cd /live && ./venv scheme-live chicken install /usr/local
# RUN cd /live && ./venv scheme-live chicken check

# RUN cd /live && ./venv scheme-live cyclone install /usr/local
# RUN cd /live && ./venv scheme-live cyclone check

RUN cd /live && ./venv scheme-live gambit install /usr/local
RUN cd /live && ./venv scheme-live gambit check

RUN cd /live && ./venv scheme-live gerbil install /usr/local
RUN cd /live && ./venv scheme-live gerbil check

# RUN cd /live && ./venv scheme-live racket install /usr/local
# RUN cd /live && ./venv scheme-live racket check

RUN cd /live && ./venv scheme-live loko install /usr/local
# RUN cd /live && ./venv scheme-live loko check

run rm -rf /live

CMD ["bash"]
