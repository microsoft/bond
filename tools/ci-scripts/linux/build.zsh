#!/bin/zsh

set -eux

SYMLINKED_HOME=$1
FLAVOR=$2
BOOST=${3:-}
COMPILER=${4:-clang}

export BOND_ROOT=/root/bond
export BUILD_ROOT=/root/build
BUILD_SCRIPTS=$BOND_ROOT/tools/ci-scripts/linux

# We set our cflags in non-standard variables because Stack chokes on some
# otherwise acceptable configurations.
case "$COMPILER" in
    clang)
        export CXX=clang++
        export CC=clang
        export BOND_CXX_FLAGS="-Qunused-arguments --system-header-prefix=boost/"
        export BOND_CC_FLAGS="-Qunused-arguments --system-header-prefix=boost/"
        ;;

    gcc)
        export CXX=g++
        export CC=gcc
        export BOND_CXX_FLAGS=
        export BOND_CC_FLAGS=
        ;;

    *) echo "Unknown compiler $COMPILER"; exit 1;;
esac

export BOND_CMAKE_FLAGS="-DBOND_USE_CCACHE=TRUE"
# Default boost root. C++ requires a specific boost and will override this.
export BOOST_ROOT=/opt/boosts/boost_1_63_0

mkdir -p $SYMLINKED_HOME $BUILD_ROOT
ln -s /root/.ccache $SYMLINKED_HOME/.ccache
ln -s /root/.stack $SYMLINKED_HOME/.stack

cd $BUILD_ROOT

case "$FLAVOR" in
    cpp-*)
        case "$FLAVOR" in
            cpp-core) BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE";;
            cpp-comm) BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_ENABLE_COMM=TRUE -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE";;
            cpp-grpc) BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DgRPC_ZLIB_PROVIDER=package";;
            *) echo "Unknown FLAVOR=$FLAVOR"; exit 1;;
        esac

        if [ ! $BOOST ]; then echo "BOOST not specified"; exit 1; fi

        export BOOST_ROOT=/opt/boosts/boost_`echo $BOOST | tr . _`

        cmake -DBOND_STACK_OPTIONS="--allow-different-user" -DCMAKE_CXX_FLAGS="$BOND_CXX_FLAGS" -DCMAKE_C_FLAGS="$BOND_CC_FLAGS" ${=BOND_CMAKE_FLAGS} /root/bond

        make --jobs 2 check
        ;;

    *)
        # Flavors that have separate scripts.
        local SCRIPT_PATH="$BUILD_SCRIPTS/build_$FLAVOR.zsh"
        if [[ -e "$SCRIPT_PATH" ]]; then
            zsh "$SCRIPT_PATH"
        else
            echo "Unknown FLAVOR $FLAVOR"
            exit 1
        fi
        ;;
esac
