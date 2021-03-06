# Revision history for haskellorls

## Unreleased

## 0.4.0.0 -- 2021-03-10

### Added

- `-T / --tabsize=COLS` option.
- `--tab-separator` option.
- `-Q / --quote-name` option.
- `-Z / --context` option.
- `-N / --literal` option.
- `-q / --hide-control-chars` and `--show-control-chars` options.
- `-k / --kibibytes` option
- Lookup `LS_BLOCK_SIZE` and `BLOCK_SIZE` environment variables to derive file block size.
- `-b / --escape` option.
- `--quoting-style=WORD` option.

### Changed

- Symblolic link block size is 0.
- Quote file name and link name dynamically by default.
- Quote a file name or a link name by double quote when the name have tab or newline.
- Escape tab and newline characters especially when output is connected to stdout.
- Output total block size as header if `-s / --size` is passed.
- Dereference symbolic link's destination file status recursively.

### Fixed

- Crash when reads no permission link.
- Out of grid alignment because of no considering full width character has double width of Latin1 character.
- Fix invalid coloring as orphan link when a symbolic link's destination is also a symbolic link.
- Escape double quote (") when quotes by double quote.
- Fix infinite loop with recursive option and dereference option when a node has a symlink which the destination is the ancestor node.

## 0.3.1.0 -- 2021-02-22

### Fixed

- Crash when lookups no permission directory.

## 0.3.0.0 -- 2021-02-22

### Added

- Add a ascii art to help header.
- Add `-L / --dereference` option.
- Add `-H / --dereference-command-line` and `--dereference-command-line-symlink-to-dir` options.
- Add `--full-time` option.
- Add `--group-directories-first`.
- Add `-C` and `-x` options.
- Add `-m` option.
- Add `--format=WORD` option.
- Add `-c` option.
- Add `-d / --directory` option.
- Add `-f` option.
- Add `-u` option.

### Changed

- Change lisence to BSD 3-Clause to release binary.
- Combine program header and program descriptions on help.

### Removed

- `-L` option which is short version of `--level`.

### Fixed

- Missing numbers output after decimal point when uses `full-iso` time style format.
- Ignoring current time zone.
- Missing a indicator when target file is symbolic link.

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
