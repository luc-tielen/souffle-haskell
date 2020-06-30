#!/bin/bash

# NOTE: This is a helper script for importing all Souffle headers into
# this repository. This is done in order to make Haskell libraries that
# use souffle-haskell "self-contained", meaning users of the packages
# using those libraries are not required to have souffle (headers) installed.

set -e

PWD=$(pwd)
ROOT_DIR=$(git rev-parse --show-toplevel)
HEADER_DIR=cbits/souffle

if [ $PWD != $ROOT_DIR ]; then
  echo "You need to run this script in the root directory of this repo! Aborting."
  exit 1
fi

git submodule update --init --recursive

cp souffle/LICENSE ${HEADER_DIR}/
find souffle -type f -name "*.h" | \
  xargs tar cf - | \
  (cd ${HEADER_DIR}; tar xf - --strip-components=2)

echo "Replace 'install-includes' in your package.yaml with the following:"
find cbits/souffle/ -type f -name "*.h" -exec echo "  - {}" \;

exit 0
