#!/bin/bash

set -eu

function setup_souffle_headers() {
    ls cbits/souffle | xargs rm -rf
    cp -r souffle/src/* cbits/souffle
}

if [ -e .git ]; then
  git submodule update --init --recursive
  setup_souffle_headers
  exit 0
fi

SOUFFLE_VERSION="1.7.1"
curl https://github.com/souffle-lang/souffle/archive/${SOUFFLE_VERSION}.tar.gz -sL -o - | tar xz
mv souffle-${SOUFFLE_VERSION} souffle
setup_souffle_headers
exit 0

