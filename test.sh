#!/bin/bash

# Behaves the same as WABT's run-tests.sh script, but overrides
# the tools to test wabt-clj.

set -e

if [[ -z $WABT_HOME ]]; then
  echo 'WABT_HOME is not set' > /dev/stderr
  exit 1
fi

project_dir="$PWD"
cd $WABT_HOME

./test/run-tests.py \
  --bindir="${project_dir}/test/wabt-bin" \
  "$@"
