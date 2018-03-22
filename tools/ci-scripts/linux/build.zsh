#!/bin/zsh

set -eux

SYMLINKED_HOME=$1
FLAVOR=$2
BOOST=${3:-}
COMPILER=${4:-clang}

BOND_ROOT=/root/bond
BUILD_ROOT=/root/build
BUILD_SCRIPTS=$BOND_ROOT/tools/ci-scripts/linux

# We set our cflags in non-standard variables because Stack chokes on some
# otherwise acceptable configurations.
case "$COMPILER" in
    clang)
        export CXX=clang++
        export CC=clang
        BOND_CXX_FLAGS="-Qunused-arguments --system-header-prefix=boost/"
        BOND_CC_FLAGS="-Qunused-arguments --system-header-prefix=boost/"
        ;;

    gcc)
        export CXX=g++
        export CC=gcc
        BOND_CXX_FLAGS=
        BOND_CC_FLAGS=
        ;;

    *) echo "Unknown compiler $COMPILER"; exit 1;;
esac

BOND_CMAKE_FLAGS="-DBOND_USE_CCACHE=TRUE"
# Default boost root. C++ requires a specific boost and will override this.
export BOOST_ROOT=/opt/boosts/boost_1_63_0

mkdir -p $SYMLINKED_HOME $BUILD_ROOT
ln -s /root/.ccache $SYMLINKED_HOME/.ccache
ln -s /root/.stack $SYMLINKED_HOME/.stack

cd $BUILD_ROOT

local SCRIPT_PATH="$BUILD_SCRIPTS/build_$FLAVOR.zsh"
if [[ -e "$SCRIPT_PATH" ]]; then
    source "$SCRIPT_PATH"
else
    echo "Unknown FLAVOR $FLAVOR"
    exit 1
fi
