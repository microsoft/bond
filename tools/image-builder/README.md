# Image Builder

This directory contains the source for a docker container that will be used for
building Bond C++ which will contain binaries of a number of Boost versions for
Ubuntu 16.04 (Xenial Xerus).

# Pre-requisites

Install the following pre-requisites:

* [Azure CLI tools][azure-cli]
* docker
* zsh

For deb-based distributions, docker and zsh can be installed via:

    $ sudo apt update && sudo apt install docker.io zsh

# Building the Image

To create the image, start in the directory containing this README.

Run the `build_image.zsh` script which will make the docker image with all
required dependencies and will compile all required versions of Boost (using
the `build_boosts.zsh` script) that will become part of the image:

    $ ./build_image.zsh

The build takes about 50 minutes on a quad-core 2.4 GHz Xeon. When it finishes,
there will be a `bond-xenial.tar.xz` archive in this directory.

# Uploading the Image

After the Image archive has been created, it needs to be uploaded to the
Azure storage account that the Travis CI builds consume from.

## Credentials

Ensure the file `~/.azure/bondbinaries.sh` has the current Azure storage
account details:

    export AZURE_STORAGE_ACCOUNT="bondbinaries"
    export AZURE_STORAGE_KEY="..."

To get the `AZURE_STORAGE_KEY`, visit the [Azure Portal][azure-portal].

## Uploading

Run `./upload_image.zsh` to upload the `bond-xenial.tar.xz` archive
produced from the build.

[azure-cli]: https://docs.microsoft.com/en-us/cli/azure/install-azure-cli
[azure-portal]: https://portal.azure.com/
