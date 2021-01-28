# Haskellorls

Haskell implementation of [exa](https://github.com/ogham/exa) flavored GNU ls.

The goal of this project is to create a software which has GNU ls compatiblities about options and output.
In addition, maybe have some options and colored output which exa has as long as keeping above compatiblities.

## Install

```sh
$ cabal install
```

## Development

Build and run.

```sh
$ cabal run haskellorls
```

## TODO

- Fix a crash when link destination is orphens link.
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
