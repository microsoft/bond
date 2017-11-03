#!/bin/bash

set -eux

pushd $HOME/docker-images
wget -N https://bondbinaries.blob.core.windows.net/bondbinaries/docker-images/bond-xenial.tar.gz
popd

time docker load -i $HOME/docker-images/bond-xenial.tar.gz
docker run -v $HOME/.ccache:/root/.ccache -v $HOME/.stack:/root/.stack -v `pwd`:/root/bond bond-xenial $HOME "$@"

# docker runs as root and may leave files in the following directories that are not readable by the travis user
sudo chown -R travis:travis $HOME/.ccache $HOME/.stack `pwd`
