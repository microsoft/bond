#!/bin/zsh

set -eux

local BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_SKIP_CORE_TESTS=TRUE"
cmake \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
    ${=BOND_CMAKE_FLAGS} \
    $BOND_ROOT

make gbc-tests

cd $BOND_ROOT/compiler
$BUILD_ROOT/compiler/build/gbc-tests/gbc-tests
