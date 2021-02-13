# Haskellorls

![CI](https://github.com/a5ob7r/haskellorls/workflows/CI/badge.svg)

Haskell implementation of [exa](https://github.com/ogham/exa) flavored GNU ls.

The goal of this project is to create a software which has GNU ls compatiblities about options and output.
In addition, maybe have some options and colored output which exa has as long as keeping above compatiblities.

![screenshort01](etc/screenshots/screenshot01.png)

## Install

```sh
$ cabal install
```

## Config

If want to use color parameters like exa default,

```sh
$ export EXA_COLORS='ur=1;33:uw=1;31:ux=1;32:ue=1;92:gr=32:gw=31:gx=33:tr=32:tw=31:tx=33:su=96:sf=96:uu=1;33:gu=1;33:'
$ haskellorls --color=auto -l
```

## Development

Build and run.

```sh
$ cabal run haskellorls
```

## Incompatibilities

### GNU ls

- Options are parsed as no ordering.
  - `Haskellorls -t -v` and `Haskellorls -v -t` causes same results.
  - It may not be useful to override same category option (e.g. sort options).
- Outputs with the column size which is specified by -w/--width even if output fd is stdout.

## TODO

- Add options for compatibility to GNU ls
  - -b, --escape
  - -c
  - -C
  - -d, --directory
  - -D, --dired
  - -f
  - --format=WORD
  - --full-time
  - --group-directories-first
  - -H, --dereference-command-line
  - --dereference-command-line-symlink-to-dir
  - --hyperlink[=WHEN]
  - -k, --kibibytes
  - -L, --dereference
  - -m
  - -N
  - -q
  - --show-control-chars
  - -Q, --quote-name
  - --quoting-style=WORD
  - -T, --tabsize=COLS
  - -u
  - -x
  - -Z, --context
- Add locale support for time style.
- Fix size printer for charactor devices.
- Add an interface model for deriving command parameters from args and options.
- Improve performances.
- Add tests.
  - Unit tests with tasty.
- Register to Hackage.
- Release binary.
- etc.

## NOTE

`Haskellorls` means Color ls in Haskell.

This is a joke software created by a beginner.
