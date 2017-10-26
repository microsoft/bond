#!/bin/zsh

set -eux
# Set this option so we can pass multiple args to cmake in a single shell variable
setopt shwordsplit

SYMLINKED_HOME=$1
FLAVOR=$2
BOOST=${3:-}

BUILD_PATH=/root/build

mkdir -p $SYMLINKED_HOME $BUILD_PATH
ln -s /root/.stack $SYMLINKED_HOME/.stack

cd $BUILD_PATH

case "$FLAVOR" in
    cpp-*)
        case "$FLAVOR" in
            cpp-core) CPP_CMAKE_ARGS="-DBOND_SKIP_GBC_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE";;
            cpp-comm) CPP_CMAKE_ARGS="-DBOND_ENABLE_COMM=TRUE -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE";;
            cpp-grpc) CPP_CMAKE_ARGS="-DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DgRPC_ZLIB_PROVIDER=package";;
            *) echo "Unknown FLAVOR=$FLAVOR"; exit 1;;
        esac

        if [ ! $BOOST ]; then echo "BOOST not specified"; exit 1; fi

        export BOOST_ROOT=/opt/boosts/boost_`echo $BOOST | tr . _`
        export CXX="ccache clang++ -Qunused-arguments --system-header-prefix=boost/"
        export CC="ccache clang -Qunused-arguments --system-header-prefix=boost/"

        ln -s /root/.ccache $SYMLINKED_HOME/.ccache

        cmake $CPP_CMAKE_ARGS /root/bond

        make --jobs 2 check
        ;;

    cs)
        # TODO: Remove build dependency on C++
        export BOOST_ROOT=/opt/boosts/boost_1_63_0
        export CXX="ccache clang++ -Qunused-arguments --system-header-prefix=boost/"
        export CC="ccache clang -Qunused-arguments --system-header-prefix=boost/"
        ln -s /root/.ccache $SYMLINKED_HOME/.ccache

        nuget restore /root/bond/cs/cs.sln

        cmake -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE /root/bond

        make gbc
        make install

        msbuild /p:Configuration=Debug /root/bond/cs/cs.sln /m
        msbuild /p:Configuration=Fields /root/bond/cs/cs.sln /m

        mono /root/NUnit.Runners.2.6.4/tools/nunit-console.exe -framework=mono-4.5 -labels \
            /root/bond/cs/test/core/bin/debug/net45/Properties/Bond.UnitTest.dll \
            /root/bond/cs/test/core/bin/debug/net45/Fields/Bond.UnitTest.dll \
            /root/bond/cs/test/internal/bin/debug/net45/Bond.InternalTest.dll
        ;;

    hs)
        # TODO: Remove build dependency on C++
        export BOOST_ROOT=/opt/boosts/boost_1_63_0
        export CXX="ccache clang++ -Qunused-arguments --system-header-prefix=boost/"
        export CC="ccache clang -Qunused-arguments --system-header-prefix=boost/"
        ln -s /root/.ccache $SYMLINKED_HOME/.ccache

        cmake -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE /root/bond

        make gbc-tests

        cd /root/bond/compiler
        $BUILD_PATH/compiler/build/gbc-tests/gbc-tests
        ;;

    *)
        echo "Unknown FLAVOR=$FLAVOR"
        exit 1
        ;;
esac
