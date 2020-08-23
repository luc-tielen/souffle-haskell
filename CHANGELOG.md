# Changelog

All notable changes to this project (as seen by library users) will be documented in this file.
The CHANGELOG is available on [Github](https://github.com/luc-tielen/souffle-haskell.git/CHANGELOG.md).

## [2.0.0] - 2020-08-23

### Added

- The `Fact` typeclass now also requires you to specify the `FactDirection`.
  This prevents inconsistent and buggy behavior when trying to use a fact in
  an invalid way (e.g. trying to add an output-only fact).
- DSL for creating Soufflé programs directly from Haskell.
  See the docs of `Language.Souffle.Experimental` for more information.

### Changed

- souffle-haskell now supports Soufflé version 2.0.1.
- `getFacts`, `findFact`, `addFact` and `addFacts` now have stricter
  constraints in their type signatures to prevent invalid usage of facts.
- `runSouffle` for both compiled and interpreted mode and `runSouffleWith`
  for interpreted mode have updated type signatures to be able to
  automatically cleanup temporary files created while interacting with Souffle.

### Removed

- `init` function for both compiled and interpreted mode. Initialization is
  now handled by the `runSouffle*` functions. This change makes automatic
  cleanup of created files possible and prevents double initialization of
  Souffle programs.
- `cleanup` function for interpreted mode, this is handled automatically now.

## [1.1.0] - 2020-07-26

### Added

- getFacts can now return facts in a fixed size array (from the array package).
- Added support for facts containing "unsigned" values.
- Added support for facts containing "float" values.

### Fixed

- The `run` function in `Language.Souffle.Interpeted` now always closes
  stdout and stderr handles of the external souffle process.

### Removed

- `Language.Souffle` module is removed since it only existed due to legacy
  reasons. This removal forces users to be explicit about the mode they are
  using souffle-haskell in (interpreted or compiled mode). If you experience
  compilation errors, rename all imports of `Language.Souffle` to
  `Language.Souffle.Compiled`.

## [1.0.0] - 2020-07-09

### Changed

- Libraries using souffle-haskell are now "self-contained": if a project
  depends on such a library, it will not require to also have Souffle installed.
- souffle-haskell now supports Soufflé version 2.0.0.
- `writeFiles` now takes an extra `FilePath` argument for writing facts to a
  certain directory.

### Deleted

- Language.Souffle.TH module is deleted because it is no longer needed anymore
  due to a change in the generated Souffle code. The generated code can now be
  correctly integrated by adding the files to `cxx-sources`
  in package.yaml / cabal file.

## [0.2.3] - 2020-05-21

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
