# pybuild - python builds python

3 ways of building python using python (refactoring/consolidation-in-progress).

Ideally should end up with 1 or maybe even 2 ways.

- builder

- pybuild

- buildpy

## builder

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

## pybuild

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

## buildpy

Intended to be the lightest weight single-script python builder with the simplest interface:

```bash
% ./buildpy.py --help
usage: buildpy.py [-h] [--debug] [--version VERSION] [--config NAME] [--reset]
                  [--optimize] [--pkgs PKG [PKG ...]] [--write]

A python builder

options:
  -h, --help            show this help message and exit
  --debug, -d           build debug python
  --version VERSION, -v VERSION
                        python version
  --config NAME, -c NAME
                        build configuration
  --reset, -r           reset build
  --optimize, -o        optimize build
  --pkgs PKG [PKG ...], -p PKG [PKG ...]
  --write, -w           write configuration
```
