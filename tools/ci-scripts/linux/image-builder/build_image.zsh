#!/bin/zsh

set -eux

docker build -t bond-xenial .
docker save bond-xenial | time gzip > bond-xenial.tar.gz
