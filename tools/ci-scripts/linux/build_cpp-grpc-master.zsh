#!/bin/zsh

set -eux

# Get gRPC's current master
pushd "$BOND_ROOT/thirdparty/grpc"
git fetch origin master
git checkout origin/master
git submodule sync --recursive
git submodule update --recursive
popd

BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DgRPC_ZLIB_PROVIDER=package"
source "$BUILD_SCRIPTS/build_cpp-common.zsh"
