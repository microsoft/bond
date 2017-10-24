#!/bin/zsh

set -eux
setopt shwordsplit

HOME=$1
FLAVOR=$2
BOOST=${3:-}

mkdir -p $HOME
ln -s /root/.stack $HOME/.stack

case "$FLAVOR" in
    cpp-*)
        case "$FLAVOR" in
            "cpp-core") CPP_CMAKE_ARGS="-DBOND_SKIP_GBC_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE";;
            "cpp-comm") CPP_CMAKE_ARGS="-DBOND_ENABLE_COMM=TRUE -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE";;
            "cpp-grpc") CPP_CMAKE_ARGS="-DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DgRPC_ZLIB_PROVIDER=package";;
            *) echo "Unknown FLAVOR=$FLAVOR"; exit 1;;
        esac

        export BOOST_ROOT=/opt/boosts/boost_`echo $BOOST | tr . _`
        export CXX="ccache clang++ -Qunused-arguments --system-header-prefix=boost/"
        export CC="ccache clang -Qunused-arguments --system-header-prefix=boost/"

        ln -s /root/.ccache $HOME/.ccache

        mkdir /root/bond_build_cpp
        cd /root/bond_build_cpp
        cmake $CPP_CMAKE_ARGS /root/bond

        make --jobs 2 check
        ;;

    "cs")
        # TODO: Remove build dependency on C++
        export BOOST_ROOT=/opt/boosts/boost_1_63_0
        export CXX="ccache clang++ -Qunused-arguments --system-header-prefix=boost/"
        export CC="ccache clang -Qunused-arguments --system-header-prefix=boost/"
        ln -s /root/.ccache $HOME/.ccache

        nuget restore /root/bond/cs/cs.sln

        mkdir /root/bond_build_cs
        cd /root/bond_build_cs
        cmake -DBOND_SKIP_GBC_TESTS=TRUE -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE /root/bond

        make gbc
        make DESTDIR=$HOME install

        export BOND_COMPILER_PATH=$HOME/usr/local/bin

        msbuild /p:Configuration=Debug /root/bond/cs/cs.sln
        msbuild /p:Configuration=Fields /root/bond/cs/cs.sln

        mono /root/NUnit.Runners.2.6.4/tools/nunit-console.exe -framework=mono-4.5 -labels \
            /root/bond/cs/test/core/bin/debug/net45/Properties/Bond.UnitTest.dll \
            /root/bond/cs/test/core/bin/debug/net45/Fields/Bond.UnitTest.dll \
            /root/bond/cs/test/internal/bin/debug/net45/Bond.InternalTest.dll
        ;;

    "hs")
        # TODO: Remove build dependency on C++
        export BOOST_ROOT=/opt/boosts/boost_1_63_0
        export CXX="ccache clang++ -Qunused-arguments --system-header-prefix=boost/"
        export CC="ccache clang -Qunused-arguments --system-header-prefix=boost/"
        ln -s /root/.ccache $HOME/.ccache

        mkdir /root/bond_build_hs
        cd /root/bond_build_hs
        cmake -DBOND_SKIP_CORE_TESTS=TRUE -DBOND_ENABLE_GRPC=FALSE /root/bond

        make gbc-tests

        cd /root/bond/compiler
        /root/bond_build_hs/compiler/build/gbc-tests/gbc-tests
        ;;

    *)
        echo "Unknown FLAVOR=$FLAVOR"
        exit 1
        ;;
esac
