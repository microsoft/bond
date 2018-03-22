#!/bin/zsh

set -eux

cmake -DBOND_STACK_OPTIONS="--allow-different-user" \
      -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" \
      -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
      ${=BOND_CMAKE_FLAGS} \
      /root/bond

make --jobs 2 check
