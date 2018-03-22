#!/bin/zsh

set -eux

if [ ! $BOOST ]; then echo "BOOST not specified"; exit 1; fi

export BOOST_ROOT=/opt/boosts/boost_`echo $BOOST | tr . _`

cmake -DBOND_STACK_OPTIONS="--allow-different-user" \
      -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" \
      -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
      ${=BOND_CMAKE_FLAGS} \
      /root/bond

make --jobs 2 check
