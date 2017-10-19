#!/bin/zsh

set -eux

docker build -t bond-xenial .
docker save bond-xenial | xz > bond-xenial.tar.xz
