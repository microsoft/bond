#!/bin/zsh

set -eux

# Define AZURE_STORAGE_ACCOUNT and AZURE_STORAGE_KEY in this file.
source ~/.azure/bondboostbinaries.sh

cd out
for i in boost-*.tar.xz; do
  print "uploading $i..."
  az storage blob upload -f $i -n $i -c bondboostbinaries
done
