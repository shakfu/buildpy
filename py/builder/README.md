# builder (python)

The original, a package extracted from another large project.. has a kitchen-sink approach.

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