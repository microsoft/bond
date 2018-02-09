#!/bin/zsh

set -eux

# This script builds the dependencies used by gbc and gbc-tests into the
# image so that we build these once.
#
# Stack is smart enough to build any missing dependencies, so if the
# pre-built dependencies in the image no longer match what bond.cabal
# specifics, we won't get an incorrect build, just a slower one.
#
# Stack doesn't have a native way of moving the .stack-work directory
# outside of the build directory, so we have to play symlink tricks here and
# also right before we perform the actual build. See also:
# https://github.com/commercialhaskell/stack/issues/1731

STACK_DEPS_COMMIT=$1

if [[ -z "$STACK_DEPS_COMMIT" ]]; then
    echo "Required STACK_DEPS_COMMIT not given. Won't be able to download source files. Be sure to set this when building the Docker image." > 2
    exit 1
fi

SAVED_STACK_WORK=/opt/stack/stack-work/

# Absolute file paths get embedded in the compiled files, so we need to
# pre-compile in the same place that the CI builds will also compile gbc.
# This ends up being done in the source tree, not in CMakes build tree.
COMPILER_PATH=$BOND_ROOT/compiler

mkdir -p "$SAVED_STACK_WORK"
mkdir -p "$COMPILER_PATH"

pushd "$COMPILER_PATH"

# Download the minimal files that Stack needs to figure out the dependent
# packages.
STACK_FILES=('bond.cabal' 'stack.yaml')
# This URI is specific to how GitHub exposes files and is tied to the main
# repository. If we need to support arbitrary repositories later, we'll
# probably want to shallow clone the repository and sparse checkout just the
# compiler/ directory.
DOWNLOAD_URI_FORMAT='https://raw.githubusercontent.com/Microsoft/bond/%s/compiler/%s'
for FILE in $STACK_FILES; do
    local FILE_URI=`printf "$DOWNLOAD_URI_FORMAT" "$STACK_DEPS_COMMIT" "$FILE"`
    wget $FILE_URI
done

# We can't use --work-dir to move .stack-work outside the current directory,
# so we use a symlink instead.
ln -s "$SAVED_STACK_WORK" .stack-work

stack setup
stack build --only-dependencies bond:gbc bond:gbc-tests

popd
rm -rf "$COMPILER_PATH"
