#!/bin/zsh

set -eux

BOOST_VERSIONS=(1.58.0 1.59.0 1.60.0 1.61.0 1.62.0 1.63.0 1.64.0)
BOOST_LIBRARIES="chrono,date_time,python,system,test,thread"

BUILD_ROOT=/root/boosts
INSTALL_ROOT=/opt
OUT=/boosts

mkdir -p $BUILD_ROOT $OUT
NUMPROCS=`cat /proc/cpuinfo | grep model\ name | wc -l`

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
  ./b2 -j $NUMPROCS install
  tar -cJf $OUT/boost-$VERSION_DOTTED.tar.xz $INSTALL_ROOT/boost_$VERSION
done
