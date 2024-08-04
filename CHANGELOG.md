# Changelog #

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased] ##

### Added ###

- [#1](https://gitlab.com/wiese28/gnucash-happy-new-year/-/issues/1) Full program test.

### Changed ###

- Renamed a lot of files and directories, for example the example files.

### Deprecated ###

### Removed ###

### Fixed ###

- More complete business entity cloning.
- [#4](https://gitlab.com/wiese28/gnucash-happy-new-year/-/issues/4) "None" target account leads to exception.

### Security ###

## [0.0.1] - 2024-07-31 ##

### Added ###

- Initial version, based on `new_book_with_opening_balances.py` from the Gnucash Python examples.
- Also duplicate business entities such as vendors, customers, employees.
- Can split opening transaction into multiple parts, possibly with different reference accounts.
- Unit tests.
- Documentation.
- CI pipeline.

### Changed ###

- A lot of code cleanup, type hints, internal documentation.
