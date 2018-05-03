#!/bin/zsh

set -eux

nuget update -self
nuget restore $BOND_ROOT/cs/cs.sln

local BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE"
cmake \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
    ${=BOND_CMAKE_FLAGS} \
    $BOND_ROOT

make gbc
make install

msbuild /p:Configuration=Debug /m $BOND_ROOT/cs/cs.sln
msbuild /p:Configuration=Fields /m $BOND_ROOT/cs/cs.sln

mono /root/NUnit.Runners.2.6.4/tools/nunit-console.exe -framework=mono-4.5 -labels \
    $BOND_ROOT/cs/test/core/bin/debug/net45/Properties/Bond.UnitTest.dll \
    $BOND_ROOT/cs/test/core/bin/debug/net45/Fields/Bond.UnitTest.dll \
    $BOND_ROOT/cs/test/internal/bin/debug/net45/Bond.InternalTest.dll
