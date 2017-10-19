FROM ubuntu:16.04

RUN apt-get update && \
    apt-get -y install \
    build-essential \
    ccache \
    clang \
    cmake \
    golang \
    haskell-stack \
    python2.7-dev \
    wget \
    zlib1g-dev \
    zsh

ADD build_boosts.zsh /root/

RUN ["zsh", "/root/build_boosts.zsh"]

VOLUME /root/.ccache /root/.stack /root/bond
