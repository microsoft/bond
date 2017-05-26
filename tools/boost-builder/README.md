# Boost Builder

This directory contains the source for a docker container that will create
binaries of a number of Boost versions for Ubuntu 14.04 (Trusty Tahr). These
binaries are useful for regression testing across those Boost releases.

# Pre-requisites

Install the following pre-requisites:

* [Azure CLI tools][azure-cli]
* docker
* zsh

For deb-based distributions, docker and zsh can be installed via:

    $ sudo apt update && sudo apt install docker.io zsh

# Building Boost binaries

To create the binaries, start in the directory containing this README.

Make the docker image:

    $ docker build -t boost-builder .

Run the image:

    $ docker run -v `pwd`/out:/boosts -t boost-builder

The build takes about fifteen minutes on a quad-core 3.3 GHz Xeon. When it
finishes, there will be a set of .tar.xz archives in `./out/`. Each archive
is meant to be unzipped from your root directory and will put, e.g., boost
1.63.0 in `/opt/boost_1_63_0`. Multiple archives can be safely extracted into
one filesystem.

# Uploading Boost binaries

After the Boost archives have been created, they need to be uploaded to the
Azure storage account that the Travis CI builds conusme them from.

## Credentials

Ensure the file `~/.azure/bondboostbinaries.sh` has the current Azure
storage account details:

    export AZURE_STORAGE_ACCOUNT="bondboostbinaries"
    export AZURE_STORAGE_KEY="..."

To get the `AZURE_STORAGE_KEY`, visit the [Azure Portal][azure-portal].

## Uploading

Run `./upload_boosts.zsh` to upload the .tar.xz files produced from the
build.

[azure-cli]: https://docs.microsoft.com/en-us/cli/azure/install-azure-cli
[azure-portal]: https://portal.azure.com/
