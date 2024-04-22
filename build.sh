#!/bin/bash

set -e

./clean.sh

mkdir bin

export BABASHKA_BBIN_BIN_DIR="$PWD/bin"

for script in wat2wie wat2wie2wasm ; do
  bbin install src/$script.clj
done
