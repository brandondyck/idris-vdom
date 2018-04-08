# Changelog

## Unreleased

### Changed

- Text nodes no longer automatically escape entities
- `render` actually diffs virtual DOMs
- Access modifiers are more sensible
- Event listeners are not updated on existing nodes

## 0.5.0 - 2018-03-01

### Added

- Some regression tests for renderer

### Changed

- Reorganized package
- Tests and example can be built without installing package
- Renderer handles all cases naïvely

## 0.4.0 - 2018-02-18

### Added

- Listeners accept `once` option
- Listeners accept `captures` option
- Listeners accept `passive` option
- Another checkbox in README.md

### Changed

- Removed `lazyConst` from example

## 0.3.0 - 2018-02-12

### Changed

- `render` takes a `Node` as root
- Updated to-do list in README.md
- Reformatted CHANGELOG.md

## 0.2.0 - 2018-02-07

### Added

- CHANGELOG.md
- Tests for existing code
- A few more low-level DOM functions

### Changed

- Use `make` instead of just Idris build system
- Rename `VirtualDOM.program` to `VirtualDOM.render`
- Probably a lot of other things

## 0.1.0

### Added

- Some things