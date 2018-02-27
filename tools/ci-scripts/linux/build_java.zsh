#!/bin/zsh

set -eux

# Install gbc.
cmake \
    -DBOND_ENABLE_JAVA=TRUE \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
    ${=BOND_CMAKE_FLAGS} \
    -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE \
    $BOND_ROOT
make install

# Try to debug failures on travis.
make gradle-plugin
find ~/.m2
cat ~/.m2/repository/org/bondlib/bond-gradle/maven-metadata-local.xml
cat ~/.m2/repository/org/bondlib/bond-gradle/*/bond-gradle*.pom

# Build and test all Java components.
make java

# Re-run cmake to pick up the paths to the core and compat jars, then run
# compat tests.
cmake \
    -DBOND_ENABLE_JAVA=TRUE \
    -DBOND_STACK_OPTIONS="--allow-different-user" \
    -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" \
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
