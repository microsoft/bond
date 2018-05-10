FROM ubuntu:16.04

# Cache folders
VOLUME /root/.ccache /root/.stack
# Source code
VOLUME /root/bond

# Enable PPAs
RUN apt-get update && \
    apt-get -y install \
    software-properties-common

# Components for Boost
RUN apt-get -y install \
    build-essential \
    python2.7-dev \
    wget \
    zlib1g-dev \
    zsh

# Build Boosts
ADD build_boosts.zsh /root/
RUN ["zsh", "/root/build_boosts.zsh"]

# Components for C++
RUN apt-get -y install \
    ccache \
    clang \
    cmake \
    golang

# Components for Haskell. See https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux
RUN wget -qO- https://get.haskellstack.org/ | sh

# Components for C#. See http://www.mono-project.com/download/#download-lin
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF && \
    echo "deb http://download.mono-project.com/repo/ubuntu xenial main" | tee /etc/apt/sources.list.d/mono-official.list && \
    apt-get update && \
    apt-get -y install \
    mono-devel \
    nuget \
    referenceassemblies-pcl
RUN nuget install NUnit.ConsoleRunner -OutputDirectory /root -Version 3.8.0 -NonInteractive -ExcludeVersion

# Components for Java
RUN add-apt-repository ppa:cwchien/gradle && \
    apt-get update && \
    apt-get -y install \
    gradle \
    openjdk-8-jdk-headless

# Build script
ENTRYPOINT ["zsh", "/root/bond/tools/ci-scripts/linux/build.zsh"]
