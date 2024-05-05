#!/bin/bash

set -ex

if [[ -z $WABT_HOME ]]; then
  echo 'WABT_HOME is not set' > /dev/stderr
  exit 1
fi

if [[ $# != 1 ]]; then
  echo 'expected name of test to run' > /dev/stderr
  exit 1
fi

testfile="$1"

wat2wasm $WABT_HOME/${testfile} -o /tmp/expected.wasm
wat2wie2wasm $WABT_HOME/${testfile} -o /tmp/actual.wasm

for name in actual expected; do
  hexdump -C /tmp/${name}.wasm > /tmp/${name}.hex
  wasm-objdump -dhxs /tmp/${name}.wasm > /tmp/${name}.dump
done

ksdiff /tmp/expected.dump /tmp/actual.dump
