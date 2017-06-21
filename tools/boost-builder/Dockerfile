FROM ubuntu:14.04.5

ADD build_boosts.zsh /root/

RUN apt-get update
RUN apt-get -y install build-essential python2.7-dev wget zsh

ENTRYPOINT ["zsh", "/root/build_boosts.zsh"]
