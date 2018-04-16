#!/bin/zsh

set -eux

BOOST_VERSIONS=(1.61.0 1.62.0 1.63.0 1.64.0 1.65.1 1.66.0 1.67.0)
BOOST_LIBRARIES="chrono,date_time,python,system,test,thread"

BUILD_ROOT=/tmp/boosts
INSTALL_ROOT=/opt/boosts

mkdir -p $BUILD_ROOT

NUMPROCS=`cat /proc/cpuinfo | grep model\ name | wc -l`
TOOLSET=gcc
COMPILER_VERSION=$TOOLSET-`$TOOLSET -dumpversion`

for VERSION_DOTTED in $BOOST_VERSIONS; do
  VERSION=`echo $VERSION_DOTTED | tr . _`
  cd $BUILD_ROOT

  wget https://downloads.sourceforge.net/project/boost/boost/$VERSION_DOTTED/boost_$VERSION.tar.bz2
  tar -xf boost_$VERSION.tar.bz2
  cd boost_$VERSION

  # --with-python needed for boost < 1.62
  ./bootstrap.sh --exec-prefix=$INSTALL_ROOT/boost_$VERSION \
                 --includedir=$INSTALL_ROOT/boost_$VERSION/include \
                 --with-libraries=$BOOST_LIBRARIES \
                 --with-python=python2.7
  ./b2 -j $NUMPROCS install toolset=$TOOLSET cxxflags="-std=c++11" linkflags="-std=c++11"
done

rm -f -r $BUILD_ROOT
