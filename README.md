[![Build Status](https://travis-ci.org/Microsoft/bond.svg?branch=master)](https://travis-ci.org/Microsoft/bond)
[![NuGet](https://img.shields.io/nuget/v/Bond.CSharp.svg?style=flat)](https://www.nuget.org/packages/Bond.CSharp/)

Bond
====

Bond is an open source, cross-platform framework for working with schematized
data. It supports cross-language serialization/deserialization and powerful
generic mechanisms for efficiently manipulating data. Bond is broadly used at
Microsoft in high scale services.

Bond is published on GitHub at [https://github.com/Microsoft/bond/](https://github.com/Microsoft/bond/).

For details, see the User's Manuals for
[C++](https://Microsoft.github.io/bond/manual/bond_cpp.html),
[C#](https://Microsoft.github.io/bond/manual/bond_cs.html) and
[Python](https://Microsoft.github.io/bond/manual/bond_py.html), and the
documentation of the compiler
[tool](https://microsoft.github.io/bond/manual/compiler.html) and
[library](https://hackage.haskell.org/package/bond).

For a discussion how Bond compares to similar frameworks see [Why Bond](https://Microsoft.github.io/bond/why_bond.html).

Dependencies
------------

The Bond repository uses Git submodules and should be cloned with the
`--recursive` flag:

    git clone --recursive https://github.com/Microsoft/bond.git

In order to build Bond you will need CMake (2.8.12+), Haskell (ghc 7.4+ and
cabal-install 1.18+) and Boost (1.54+). The core Bond C++ library can be used
with C++03 compilers, although Python support, unit tests and various examples
require some C++11 features.

Following are specific instructions for building on various platforms.

### Linux

Bond can be built with Clang (3.4+) or GNU C++ (4.7+). We recommend the latest
version of Clang as it's much faster with template-heavy code like Bond.

Run the following commands to install the minimal set of packages needed to
build the core Bond library on Ubuntu 14.04:

    sudo apt-get install \
        clang \
        cmake \
        zlib1g-dev \
        ghc \
        cabal-install \
        libboost-dev \
        libboost-thread-dev

    cabal update
    cabal install cabal-install

In the root `bond` directory run:

    mkdir build
    cd build
    cmake ..
    make
    sudo make install

The `build` directory is just an example. Any directory can be used as the build
destination.

In order to build all the C++ and Python tests and examples, a few more
packages are needed:

    sudo apt-get install \
        python2.7-dev \
        libboost-date-time-dev \
        libboost-test-dev \
        libboost-python-dev

    cabal install happy

Running the following command in the build directory will build and execute all
the tests and examples:

    make --jobs 8 check

(The unit tests are large so you may want to run 4-8 build jobs in parallel,
assuming you have enough memory.)

### OS X

Install Xcode and then run the following command to install the required
packages using Homebrew ([http://brew.sh/](http://brew.sh/)):

    brew install \
        cmake \
        ghc \
        cabal-install \
        boost \
        boost-python

(boost-python is optional and only needed for Python support.)

Update the cabal package database and install `happy` (only needed for tests):

    cabal update
    cabal install happy

Bond can be built on OS X using either standard \*nix makefiles or Xcode. In
order to generate and build from makefiles, in the root `bond` directory run:

    mkdir build
    cd build
    cmake ..
    make
    sudo make install

Alternatively, you can generate Xcode projects by passing the `-G Xcode` option
to cmake:

    cmake -G Xcode ..

You can build and run unit tests by building the `check` target in Xcode or by
running make in the build directory:

    make --jobs 8 check

Note that if you are using Homebrew's Python, you'll need to build
boost-python from source:

    brew install --build-from-source boost-python

and tell cmake the location of Homebrew's libpython by setting the
`PYTHON_LIBRARY` variable, e.g.:

    cmake .. \
        -DPYTHON_LIBRARY=/usr/local/Cellar/python/2.7.9/Frameworks/Python.framework/Versions/2.7/lib/libpython2.7.dylib

### Windows

[![Build Status](https://ci.appveyor.com/api/projects/status/github/Microsoft/bond?svg=true&branch=master)](https://ci.appveyor.com/project/sapek/bond/branch/master)

Install the following tools:

- Visual Studio 2013 or 2015
- CMake ([http://www.cmake.org/download/](http://www.cmake.org/download/))
- Haskell Platform ([http://haskell.org/platform/](http://haskell.org/platform/))

If you are building on a network behind a proxy, set the environment variable
`HTTP_PROXY`, e.g.:

    set HTTP_PROXY=http://your-proxy-name:80

Update the cabal package database:

    cabal update

Now you are ready to build the C# version of Bond. Open the solution file
`cs\cs.sln` in Visual Studio and build as usual. The C# unit tests can
also be run from within the solution.

The C++ and Python versions of Bond additionally require:

- Boost 1.54+ ([http://www.boost.org/users/download/](http://www.boost.org/users/download/))
- Python 2.7 ([https://www.python.org/downloads/](https://www.python.org/downloads/))

You may need to set the environment variables `BOOST_ROOT` and `BOOST_LIBRARYDIR`
to specify where Boost and its pre-built libraries for your environment can be
found, e.g.:

    set BOOST_ROOT=D:\boost_1_57_0
    set BOOST_LIBRARYDIR=D:\boost_1_57_0\lib64-msvc-12.0

The core Bond library and most examples only require Boost headers. The
pre-built libraries are only needed for unit tests and Python support. If Boost
or Python libraries are not found on the system, then some tests and examples will
not be built.

In order to generate a solution to build the C++ and Python versions with Visual
Studio 2013 run the following commands from the root `bond` directory:

    mkdir build
    cd build
    cmake -G "Visual Studio 12 2013 Win64" ..

Instead of `cmake` you can also use `cmake-gui` and specify configuration
settings in the UI. This configuration step has to be performed only once. From
then on you can use the generated solution `build\bond.sln` from Visual Studio
or build from command line using `cmake`:

    set PreferredToolArchitecture=x64
    cmake --build . --target
    cmake --build . --target INSTALL

In order to build and execute the unit tests and examples run:

    cmake --build . --target check -- /maxcpucount:8

Setting `PreferredToolArchitecture=x64` selects the 64-bit toolchain which
dramatically improves build speed. (The Bond unit tests are too big to build
with 32-bit tools.) This variable works for Visual Studio 2013 or 2015. For
Visual Studio 2012 set the following environment variable instead:

    set _IsNativeEnvironment=true
