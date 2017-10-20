#!/bin/zsh

set -eux

# Define AZURE_STORAGE_ACCOUNT and AZURE_STORAGE_KEY in this file.
source ~/.azure/bondbinaries.sh

az storage blob upload -f bond-xenial.tar.xz -n docker-images/bond-xenial.tar.xz -c bondbinaries
