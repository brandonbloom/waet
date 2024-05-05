#!/bin/bash

# Behaves the same as WABT's run-tests.sh script, but overrides
# the tools to test waet.

set -e

if [[ -z $WABT_HOME ]]; then
  echo 'WABT_HOME is not set' > /dev/stderr
  exit 1
fi

cd $WABT_HOME

./test/run-tests.py \
  --bindir="$WAET_HOME/test/wabt-bin" \
  "$@"
