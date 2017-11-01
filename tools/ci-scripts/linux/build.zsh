#!/bin/zsh

set -eux

SYMLINKED_HOME=$1
FLAVOR=$2
BOOST=${3:-}
COMPILER=${4:-clang}

# We set our compilers and cflags in non-standard variables because Stack chokes
# on some otherwise acceptable configurations. (e.g., CC="ccache gcc")
case "$COMPILER" in
    clang)
        export CXX_COMPILER=clang++
        export CXX_FLAGS="-Qunused-arguments --system-header-prefix=boost/"
        export CC_COMPILER=clang
        export CC_FLAGS="-Qunused-arguments --system-header-prefix=boost/"
        ;;

    gcc)
        export CXX_COMPILER=g++
        export CXX_FLAGS=
        export CC_COMPILER=gcc
        export CC_FLAGS=
        ;;

    *) echo "Unknown compiler $COMPILER"; exit 1;;
esac

export BOND_CMAKE_FLAGS="-DBOND_USE_CCACHE=TRUE"

BUILD_PATH=/root/build
BUILD_SCRIPTS=/root/bond/tools/ci-scripts/linux

mkdir -p $SYMLINKED_HOME $BUILD_PATH
ln -s /root/.stack $SYMLINKED_HOME/.stack

cd $BUILD_PATH

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
        export CXX=$CXX_COMPILER
        export CC=$CC_COMPILER

        ln -s /root/.ccache $SYMLINKED_HOME/.ccache

        cmake -DBOND_STACK_OPTIONS="--allow-different-user" -DCMAKE_CXX_FLAGS="$CXX_FLAGS" -DCMAKE_C_FLAGS="$CC_FLAGS" ${=BOND_CMAKE_FLAGS} /root/bond

        make --jobs 2 check
        ;;

    cs)
        # TODO: Remove build dependency on C++
        export BOOST_ROOT=/opt/boosts/boost_1_63_0
        export CXX=$CXX_COMPILER
        export CC=$CC_COMPILER
        ln -s /root/.ccache $SYMLINKED_HOME/.ccache

        nuget restore /root/bond/cs/cs.sln

        BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE"
        cmake -DBOND_STACK_OPTIONS="--allow-different-user" -DCMAKE_CXX_FLAGS="$CXX_FLAGS" -DCMAKE_C_FLAGS="$CC_FLAGS" ${=BOND_CMAKE_FLAGS} /root/bond

        make gbc
        make install

        msbuild /p:Configuration=Debug /m /root/bond/cs/cs.sln
        msbuild /p:Configuration=Fields /m /root/bond/cs/cs.sln

        mono /root/NUnit.Runners.2.6.4/tools/nunit-console.exe -framework=mono-4.5 -labels \
            /root/bond/cs/test/core/bin/debug/net45/Properties/Bond.UnitTest.dll \
            /root/bond/cs/test/core/bin/debug/net45/Fields/Bond.UnitTest.dll \
            /root/bond/cs/test/internal/bin/debug/net45/Bond.InternalTest.dll
        ;;

    java)
        # TODO: Remove build dependency on C++
        export BOOST_ROOT=/opt/boosts/boost_1_63_0
        export CXX=$CXX_COMPILER
        export CC=$CC_COMPILER
        exec $BUILD_SCRIPTS/build_java.zsh
        ;;

    hs)
        # TODO: Remove build dependency on C++
        export BOOST_ROOT=/opt/boosts/boost_1_63_0
        export CXX=$CXX_COMPILER
        export CC=$CC_COMPILER
        ln -s /root/.ccache $SYMLINKED_HOME/.ccache

        BOND_CMAKE_FLAGS="$BOND_CMAKE_FLAGS -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE"
        cmake -DBOND_STACK_OPTIONS="--allow-different-user" -DCMAKE_CXX_FLAGS="$CXX_FLAGS" -DCMAKE_C_FLAGS="$CC_FLAGS" ${=BOND_CMAKE_FLAGS} /root/bond

        make gbc-tests

        cd /root/bond/compiler
        $BUILD_PATH/compiler/build/gbc-tests/gbc-tests
        ;;

    *)
        echo "Unknown FLAVOR=$FLAVOR"
        exit 1
        ;;
esac
