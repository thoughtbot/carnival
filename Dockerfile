FROM heroku/cedar:14

ENV DEBIAN_FRONTEND noninteractive
ENV LANG en_US.UTF-8
ENV PATH $PATH:/root/.local/bin

RUN mkdir -p /app/user
WORKDIR /app/user

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 \
  && echo 'deb http://download.fpcomplete.com/ubuntu trusty main' > \
    /etc/apt/sources.list.d/fpco.list \
  && apt-get update \
  && apt-get install -y \
    coffeescript \
    postgresql-client \
    stack \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY stack.yaml .
RUN stack setup

COPY *.cabal ./
RUN stack build --dependencies-only

COPY . /app/user
RUN stack build
RUN stack install
RUN cp /root/.local/bin/* .
RUN rm -rf /app/user/.stack-work
