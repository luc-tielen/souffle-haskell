
# Changelog

All notable changes to this project will be documented in this file.
The CHANGELOG is available on [Github](https://github.com/luc-tielen/souffle-haskell.git/CHANGELOG.md).


## [0.1.0] - 2019-12-21
### Added

- Added Marshal instance for lazy and strict Text

### Changed

- getFacts can now return a vector instead of a list, based on type inference.
  This allows for a more efficient representation in memory as well
  as being able to allocate all needed memory once before collecting facts.


## [0.0.1] - 2019-10-23
### Added

- Initial version of the library
