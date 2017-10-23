#!/bin/zsh

set -eux

docker build -t bond-xenial .
docker save bond-xenial | time xz -T0 > bond-xenial.tar.xz
