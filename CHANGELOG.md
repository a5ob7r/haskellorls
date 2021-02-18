# Revision history for haskellorls

## Unreleased

- Add a ascii art to help header

## 0.2.0.0 -- 2021-02-17

### Added

- Add help messages to some options.
- Add `-n / --numeric-uid-gid` option.
- Add `-I / --ignore=PATTERN` and `--hide=PATTERN` option.
- Add `-R / --recursive` and `-L / --level=N` options.
- Add `--author` option.
- Add `-s / --size` option.
- Add total size header output for directory.
- Add `--icons` option.
- Add `-v` option (natual sort).
- Add `-X` option (extension sort).
- Add `-t` option (time sort).
- Add `-U` option (none sort).
- Add `-S` option (size sort).
- Add `--tree` option.

### Fixed

- Fix crashing when passes no existence filepath.
- Fix imcompatible file size format output against GNU ls.
- Fix slow outputs partially when output text is so large.
- Fix time option parser to raise error about invalid values.
- Fix always return 0 as exit code.
- Fix sort order using time.
- Fix name sort order to be compatible to GNU ls.

## 0.1.0.0 -- 2021-02-01

First version. But this have many missing functionalities about GNU ls compatibilities.
