#!/bin/zsh

set -eux

curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
mv microsoft.gpg /etc/apt/trusted.gpg.d/microsoft.gpg
sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod xenial main" > /etc/apt/sources.list.d/dotnetdev.list'

sudo apt-get update
sudo apt-get install apt-transport-https
sudo apt-get install dotnet-sdk-2.1.105

# nuget update -self
# nuget restore $BOND_ROOT/cs/cs.sln
dotnet restore $BOND_ROOT/cs/cs.sln

local BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE"
cmake \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
    ${=BOND_CMAKE_FLAGS} \
    $BOND_ROOT

make gbc
make install

# msbuild /p:Configuration=Debug /m $BOND_ROOT/cs/cs.sln
# msbuild /p:Configuration=Fields /m $BOND_ROOT/cs/cs.sln
dotnet msbuild /p:Configuration=Debug /m $BOND_ROOT/cs/cs.sln
dotnet msbuild /p:Configuration=Fields /m $BOND_ROOT/cs/cs.sln

# dotnet test $BOND_ROOT/cs/test/core/Core.csproj
dotnet test cs/test/coreNS10/CoreNS10.csproj -c Debug
dotnet test cs/test/coreNS10/CoreNS10.csproj -c Fields
dotnet test cs/test/internal/Internal.csproj
dotnet test cs/test/grpc/grpc.csproj

# mono /root/NUnit.Runners.2.6.4/tools/nunit-console.exe -framework=mono-4.5 -labels \
#     $BOND_ROOT/cs/test/core/bin/debug/Properties/net45/Bond.UnitTest.dll \
#     $BOND_ROOT/cs/test/core/bin/debug/Fields/net45/Bond.UnitTest.dll \
#     $BOND_ROOT/cs/test/internal/bin/debug/net45/Bond.InternalTest.dll
