# Haskellorls

Haskell implementation of [exa](https://github.com/ogham/exa) flavored GNU ls.

The goal of this project is to create a software which has GNU ls compatiblities about options and output.
In addition, maybe have some options and colored output which exa has as long as keeping above compatiblities.

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

## TODO

- Add locale support for time style.
- Add extra sort method.
- Fix name sort for GNU ls compatibility.
- Add inode field for grid and long style layout.
- Fix size size printer for charactor devices.
- Add -F and --file-type options.
- Add -I or --ignore=PATTERN and --hide=PATTERN options.
- Integrate grid and long style rendering logic.
- Add an interface model for deriving command parameters from args and options.
- Add a printer to show link destination of symbolic link node.
- Improve performances.
- Add tests.
  - E2E tests with bats.
  - Unit tests with tasty.
- etc.

## NOTE

`Haskellorls` means Color ls with Haskell.

This is a joke software created by a beginner.
