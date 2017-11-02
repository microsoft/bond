#!/bin/zsh

set -eux

BOND_ROOT=/root/bond

# Install gbc.
cmake \
    -DBOND_ENABLE_JAVA=True \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$CXX_FLAGS" -DCMAKE_C_FLAGS="$CC_FLAGS" \
    ${=BOND_CMAKE_FLAGS} \
    -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE \
    $BOND_ROOT
make install

# Build and test all Java components.
make java

# Re-run cmake to pick up the paths to the core and compat jars, then run
# compat tests.
cmake \
    -DBOND_ENABLE_JAVA=True \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$CXX_FLAGS" -DCMAKE_C_FLAGS="$CC_FLAGS" \
    ${=BOND_CMAKE_FLAGS} \
    -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE \
    $BOND_ROOT
make --jobs 2 check

# Run Java examples.
cd $BOND_ROOT/examples/java/core
for example in *; do
    pushd $example
    echo "running examples/java/core/$example"
    gradle run
    popd
done
