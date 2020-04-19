
# Changelog

All notable changes to this project will be documented in this file.
The CHANGELOG is available on [Github](https://github.com/luc-tielen/souffle-haskell.git/CHANGELOG.md).

## [0.2.0] - 2020-04-22
### Added

- Added Interpreted module. The location of the souffle interpreter and datalog input directory
  can be controlled set via `SOUFFLE_BIN` and `DATALOG_DIR`. When invoked with the `runSouffle`,
  if not given, souffle is looked up via `whereis souffle` and the input datalog files are searched,
  in the active directory. When invoked with `runSouffleWith` the path for the datalog program needs
  to be given. NOTE: For this mode the datalog program must render the output csv using
  TAB characters as separators.

### Changed

- Introduced Language.Souffle.Class module as separation of the typeclass and the
  Compiled module made the implementation of the Interpreted module easier.
- Marshaling is now based on the free monad approach. The implementors of the
  Souffle type class needs to give an interpretation of the Marshal expressions too.

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
