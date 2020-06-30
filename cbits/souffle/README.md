# Note on this directory

This directory contains a copy of all souffle headers. This is done in order
to make Haskell libraries that use souffle-haskell "self-contained", meaning
users of packages using those libraries do not need to have souffle (headers)
installed.

The header files should only be updated by running `./scripts/import_headers.sh`
in the root directory of the souffle-haskell repo.
