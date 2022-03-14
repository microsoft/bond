#!/bin/zsh

set -eux

nuget update -self
nuget restore $BOND_ROOT/cs/cs.sln

local BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE"
cmake \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
    ${=BOND_CMAKE_FLAGS} \
    $BOND_ROOT

make gbc
make install

msbuild /p:Configuration=Debug /m $BOND_ROOT/cs/cs.sln
msbuild /p:Configuration=Fields /m $BOND_ROOT/cs/cs.sln

mono /root/NUnit.ConsoleRunner/tools/nunit3-console.exe -framework=mono -labels=All \
    $BOND_ROOT/cs/test/core/bin/debug/Properties/net45/Bond.UnitTest.dll \
    $BOND_ROOT/cs/test/core/bin/debug/Fields/net45/Bond.UnitTest.dll \
    $BOND_ROOT/cs/test/coreNS10/bin/debug/Properties/net45/Bond.UnitTestCoreNS10.dll \
    $BOND_ROOT/cs/test/coreNS10/bin/debug/Fields/net45/Bond.UnitTestCoreNS10.dll \
    $BOND_ROOT/cs/test/internal/bin/debug/net45/Bond.InternalTest.dll
