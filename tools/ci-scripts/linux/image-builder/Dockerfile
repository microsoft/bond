ARG UBUNTU_VERSION=18.04
FROM ubuntu:$UBUNTU_VERSION
# https://docs.docker.com/engine/reference/builder/#understand-how-arg-and-from-interact
ARG UBUNTU_VERSION

# Cache folders
VOLUME /root/.ccache /root/.stack
# Source code
VOLUME /root/bond

# Enable PPAs
RUN apt-get update && \
    apt-get -y install \
    apt-transport-https \
    ca-certificates \
    software-properties-common

# Components for gbc
RUN apt-get -y install \
    happy

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
    cmake

# Components for Java
RUN add-apt-repository ppa:cwchien/gradle && \
    apt-get update && \
    apt-get -y install \
    gradle-6.8.3 \
    openjdk-8-jdk-headless

# Components for Haskell. See https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux
RUN wget -qO- https://get.haskellstack.org/ | sh

# Components for ghc
RUN add-apt-repository ppa:hvr/ghc -y && \
    apt-get update && \
    apt-get -y install ghc-8.6.5

# Install more recent git
RUN add-apt-repository ppa:git-core/ppa -y && \
    apt-get update && \
    apt-get install --no-install-recommends git -y


# we need to set bash to be able to use if statement
SHELL ["/bin/bash", "-c"]

# Components for C#. See http://www.mono-project.com/download/#download-lin
RUN if [ "$UBUNTU_VERSION" = "18.04" ] || [ "$UBUNTU_VERSION" = "20.04" ] ; then apt install gnupg ca-certificates; fi

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF

RUN if [[ "$UBUNTU_VERSION" = "16.04" ]] ; then echo "deb https://download.mono-project.com/repo/ubuntu stable-xenial main"; \
    elif [[ "$UBUNTU_VERSION" = "18.04" ]] ; then echo "deb https://download.mono-project.com/repo/ubuntu stable-bionic main"; \
    elif [[ "$UBUNTU_VERSION" = "20.04" ]] ; then echo "deb https://download.mono-project.com/repo/ubuntu stable-focal main"; fi \
    | tee /etc/apt/sources.list.d/mono-official.list && apt-get update && apt-get -y install \
    mono-devel \
    nuget \
    referenceassemblies-pcl
RUN nuget install NUnit.ConsoleRunner -OutputDirectory /root -Version 3.8.0 -NonInteractive -ExcludeVersion

# Build script
ENTRYPOINT ["zsh", "/root/bond/tools/ci-scripts/linux/build.zsh"]
