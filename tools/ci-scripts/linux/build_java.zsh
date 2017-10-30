#!/bin/zsh

set -eux

BOND_ROOT=/root/bond
cmake -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE $BOND_ROOT
make install
make java

# Re-run cmake to pick up the paths to the core and compat jars.
cmake -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE $BOND_ROOT
make --jobs 2 check

# Run Java examples.
cd $BOND_ROOT/examples/java/core
for example in *; do
    pushd $example
    echo "running examples/java/core/$example"
    gradle run
    popd
done
