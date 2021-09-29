#!/bin/bash

SCRIPT_DIR=$(dirname $BASH_SOURCE)

function cleanup_previous_souffle_files() {
  echo "Cleaning up previously used header files in cbits/souffle/"
  find $SCRIPT_DIR/../cbits/souffle/* -type d | xargs rm -r
  find $SCRIPT_DIR/../cbits/souffle/* | grep -v README.md | xargs rm
}

function copy_license() {
  echo "Copying over souffle LICENSE file"
  cp $SCRIPT_DIR/../souffle/LICENSE $SCRIPT_DIR/../cbits/souffle/LICENSE
}

function copy_headers() {
  INCLUDE_PATHS=$(clang++ -MM -H ${SCRIPT_DIR}/analyze_headers.cpp -I $SCRIPT_DIR/../souffle/src/include 2>&1 | \
    grep souffle | \
    awk '{ print $2; }' | \
    grep -E "*.h$")
  INSTALL_HEADERS=$(clang++ -MM -H ${SCRIPT_DIR}/analyze_headers.cpp -I $SCRIPT_DIR/../souffle/src/include 2>&1 | \
    grep souffle | \
    awk -F 'include/' '{ print $2; }' | \
    grep -E "*.h$")

  echo "Copying over minimal set of required header files to cbits/souffle/"
  for HEADER in $INCLUDE_PATHS; do
    DESTINATION=cbits/$(echo $HEADER | awk -F 'include/' '{print $2;}')
    DESTINATION_DIR=$(dirname $DESTINATION)
    mkdir -p $DESTINATION_DIR
    cp $HEADER $DESTINATION
  done
}

function show_required_includes() {
  echo "Add the following to 'install-includes' section in package.yaml:"
  echo

  echo "install-includes:"
  for HEADER in $INSTALL_HEADERS; do
    echo "  - $HEADER"
  done
}

cleanup_previous_souffle_files
copy_license
copy_headers
show_required_includes

exit 0
