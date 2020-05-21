
# Changelog

All notable changes to this project (as seen by library users) will be documented in this file.
The CHANGELOG is available on [Github](https://github.com/luc-tielen/souffle-haskell.git/CHANGELOG.md).

## [0.2.2] - 2020-05-21
### Changed

- Optimize performance when marshalling and unmarshalling facts.


## [0.2.2] - 2020-04-30
### Changed

- Fix compile time issue when generically deriving `Marshal` typeclass
  for data types with more than 3 fields.


## [0.2.1] - 2020-04-25
### Changed

- Trimmed dependencies to make the library more lightweight.


## [0.2.0] - 2020-04-22
### Added

- Added Language.Souffle.Interpreted module for running Souffle programs in interpreted mode.
  NOTE: For this mode the CSV fact files must use TAB (`'\t'`) characters as separators.
- In interpreted mode, you can configure where the library looks for datalog files or where
  the souffle executable is located. For more information, see the `runSouffle` and `runSouffleWith`
  functions in the Language.Souffle.Interpreted module.

### Changed

- Introduced Language.Souffle.Class module as separation of the typeclass and the
  Language.Souffle.Compiled module to offer a uniform API in both interpreted and compiled mode.


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
