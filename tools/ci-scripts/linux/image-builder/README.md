# Image Builder

This directory contains the source for a Docker image that is used for
building Bond.

The image is based on Ubuntu 18.04 (Bionic Beaver) and contains:

* pre-installed build tools for the various languages Bond supports,
* binaries of a number of Boost versions for the C++ and Python builds, and
* an entrypoint that can be used to drive a build.

# Automatic Builds

![Docker Ubuntu image build status](https://msazure.visualstudio.com/_apis/public/build/definitions/b32aa71e-8ed2-41b2-9d77-5bc261222004/14573/badge)

A new image is automatically built when the master branch of the main Bond
repository (https://github.com/microsoft/bond) is changed. These images are
then pushed to the bondciimages.azurecr.io/ubuntu-1804 (also bondciimages.azurecr.io/ubuntu-1604) container repository.
The Travis CI builds pull a fixed version of the image specified in
`.travis.yml` and use that.

The image is built on a Microsoft VSTS instance. Its status is shown in the
image above. Currently, access to the results of this build are limited to
Microsoft employees. If you need details about a given build, please
[open an issue](https://github.com/microsoft/bond/issues/new) and one of the
maintainers from Microsoft can get you the details.

## Cleanup

![Docker Ubuntu umage cleanup status](https://msazure.visualstudio.com/_apis/public/build/definitions/b32aa71e-8ed2-41b2-9d77-5bc261222004/21361/badge)

Not all of the images built automatically are used by CI builds. We have
another VSTS build that runs a cleanup script to delete unused images. It
keeps images that are referenced in `.travis.yml` in the last 10 commits of
the master branch, the past two week's of commits, and any tags in the main
GitHub repository. It also keeps any image younger than seven days.

# Manual Builds

You can build and use the image locally.

## Pre-requisites

Install the following pre-requisites:

* Docker
* zsh

For deb-based distributions, Docker and zsh can be installed via:

    $ sudo apt update && sudo apt install docker.io zsh
    
zsh is only needed to debug and work on `build_boost.zsh` locally.

## Building the Image

To create the image, start in the directory containing this README and run:

    $ docker build --no-cache -t bond-ubuntu-1804 --build-arg UBUNTU_VERSION="18.04" .

This will make the Docker image with all the required dependencies and will
compile all the required versions of Boost (using the `build_boosts.zsh`
script) that will become part of the image.

The build takes about 20 minutes on a quad-core 3.3 GHz Skylake Xeon. When
it finishes, there will be a new Docker image with the tag
`bond-ubuntu-1804` in your collection of Docker images:

    $ docker images

## Using the Image

You can build Bond inside the container:

    $ docker run \
        -v path/to/your/bond/repository:/root/bond \
        bond-ubuntu-1804 \
        ~ \
        cpp-core \
        1.64.0 \
        gcc

Be sure to change `path/to/your/bond/repository`. The `FLAVOR`, `BOOST`, and
`COMPILER` arguments (in this example `cpp-core`, `1.64.0`, and `gcc`,
respectively) can be adjusted as needed. See `build.zsh` for details.
