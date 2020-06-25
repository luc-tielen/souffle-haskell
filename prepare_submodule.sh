#!/bin/bash

set -eu

if [ -e .git ]; then
  git submodule update --init --recursive
  exit 0
fi

SOUFFLE_VERSION="1.7.1"
curl https://github.com/souffle-lang/souffle/archive/${SOUFFLE_VERSION}.tar.gz -sL -o - | tar xz
mv souffle-${SOUFFLE_VERSION} cbits/souffle
exit 0

