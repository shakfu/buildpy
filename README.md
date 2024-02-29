# pybuild - a bunch of python builders

This project contains several python builders from source (3 using python) and another in development using golang.


## python builds python

3 ways of building python from source using python: 

1. `builder`: the original, a package extracted from another large project.. has a kitchen-sink approach 

2. `pybuild`: a subsequent attempt at a lighter-weight package, extracted from (1) with only the build features.

3. `buildpy`: lightest-weight single script/module, a 'modern' rewrite given collective experience of (1) and (2): the future.

Ideally one should end up with 1 or maybe even 2 ways. 

(refactoring/consolidation-in-progress)

### builder

Heavyweight, full featured, building, codesigning, packaging solution, extracted from [py-js](https://github.com/shakfu/py-js)

Needs quite a bit of cleanup and refactoring to make it a general tool.

Either use `make` interface:

```bash

make python-shared

```

or the package's argparse interface:

```bash
% python3 -m builder
usage: python3 -m builder [-h] [-v]  ...

builder: builds python from source.

options:
  -h, --help     show this help message and exit
  -v, --version  show program's version number and exit

subcommands:
  valid subcommands

                 additional help
    dep          dependency commands
    help         display online help
    package      package, sign and release external
    python       download and build python from src
```

### pybuild

Lighter weight python builder as a single module or minimal package, with just the build functionality extracted from `builder`.

Simplified argparse interface:

```bash
% ./pybuild.py --help
usage: pybuild.py [-h] [-v] {all,framework,shared,static} ...

pybuild: builds python from src

options:
  -h, --help            show this help message and exit
  -v, --version         show program's version number and exit

subcommands:
  valid subcommands

  {all,framework,shared,static}
                        additional help
    all                 build all python variations
    framework           build framework python
    shared              build shared python
    static              build static python
```

### buildpy

The lightest weight single-script python builder with the simplest interface. Only handles python 3.11 - 3.12 and has a builtin congifuration system which handles differences between build variants efficiently.

```bash
% ./buildpy.py --help
usage: buildpy.py [-h] [-a CFG [CFG ...]] [-c NAME] [-d] [-o]
                  [-p PKG [PKG ...]] [-r] [-v VERSION] [-w]

A python builder

options:
  -h, --help            show this help message and exit
  -a CFG [CFG ...], --cfg-opts CFG [CFG ...]
                        add config options
  -c NAME, --config NAME
                        build configuration (default: shared_mid)
  -d, --debug           build debug python
  -o, --optimize        optimize build
  -p PKG [PKG ...], --pkgs PKG [PKG ...]
                        install pkgs
  -r, --reset           reset build
  -v VERSION, --version VERSION
                        python version (default: 3.11.7)
  -w, --write           write configuration
```

## go

### buildpy (go-edition)

Under development: a translation of `buildpy` to golang. (Hey, Why not?!)

