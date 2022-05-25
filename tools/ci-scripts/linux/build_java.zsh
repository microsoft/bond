#!/bin/zsh

set -eux

# Java needs git tags to detect the version it's building. If we haven't
# released in a while, the last tag might be many commits back, so we'll fetch
# the entire history here.
pushd $BOND_ROOT
git config --global --add safe.directory $BOND_ROOT
git fetch --unshallow --tags --no-recurse-submodules
popd

# Install gbc.
cmake \
    -DBOND_ENABLE_JAVA=TRUE \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
    ${=BOND_CMAKE_FLAGS} \
    -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE \
    $BOND_ROOT
make install

# Build and test all Java components.
make java

# Re-run cmake to pick up the paths to the core and compat jars, then run
# compat tests.
cmake \
    -DBOND_ENABLE_JAVA=TRUE \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
    ${=BOND_CMAKE_FLAGS} \
    -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE \
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
