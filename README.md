# pybuild - a bunch of python3 builders

This project explores different ways to programmatically configure and build python from source.

The general steps are as follows:

1. Create local build environment / project which consists of the following folder structure

```
./build/
./build/downloads/
./build/src/
./build/install/
```

2. Download, build, and install python dependencies {openssl, bzip2, xz, ..} into build project
3. Download the source code of a particular version of python3 from python.org
4. Configure python3 build using configure options and custom `Setup.local` file
5. Build and install python3 into build project
6. Clean or remove extraneous libraries, extensions, modules
7. Zip standard library

The end product is especially useful for integration in an other compiled project or plugin via embedding.

Features of `buildpy` variants by language:

| Features                   |  python | golang   | rust     | haskell  | swift    |
| :------------------------- | :------:| :------: | :------: | :------: | :------: |
| Create Build Env           | x       | x        | x        | x        |          |
| Build Python Dependencies  | x       | x        | x        | x        |          |
| Configure Python Build     | x       | x        | x        |          |          |
| Build/Install Python       | x       | x        | x        |          |          |
| Clean Python Build         | x       | x        | x        |          |          |
| Zip python library         | x       | x        | x        |          |          |



## The Configuration Format

```python
class Config:
  name: str
  version: str
  headers: list[str]
  static: list[str]
  shared: list[str]
  disabled: list[str]
  config_opts: list[str]
  packages: list[str]
  remove_patterns: list[str]
  optimize: bool
  debug: bool
```

## The builders

### builder (python)

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

### pybuild (python)

A subsequent attempt at a lighter-weight package, extracted from (1) with only the build features.

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

### buildpy (python)

Lightest-weight single script/module, a 'modern' rewrite given collective experience of (1) and (2). The future, so to speak.

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


### buildpy (go-edition)

A golang version of `buildpy`.


```bash
% ./buildpy --help
Builds python from source

Usage:
  buildpy [command]

Available Commands:
  build       Build python from source
  completion  Generate the autocompletion script for the specified shell
  config      Manage python configuration
  deps        Build and manage python dependencies
  help        Help about any command

Flags:
  -h, --help   help for buildpy

Use "buildpy [command] --help" for more information about a command.
```

Mostly mirrors the python version except for the following:

- Round-trip serialization/deserialization from/to yaml


### buildpy (rust-edition)

```bash
% ./buildpy --help
Builds python from source

Usage: buildpy [OPTIONS]

Options:
  -p, --pyversion <PYVERSION>  Python version [default: 3.11.8]
  -c, --config <CONFIG>        Config name [default: static_max]
      --opts <OPTS>            Config options
      --pkgs <PKGS>            Install python packages
  -o, --optimize               Optimize build
  -g, --git                    Use git to download code
  -r, --reset                  reset before build
  -d, --demo                   Run Demo
  -j, --jobs <JOBS>            Parallel build jobs [default: 4]
  -h, --help                   Print help
  -V, --version                Print version
sa@minx buildpy %
```

A rust version of `buildpy`.

Mostly mirrors the python version except for the following:

- Round-trip serialization/deserialization from/to json


### buildpy (swift-edition)

```bash
% ./buildpy --help
USAGE: buildpy [--version <version>] [--config <config>] [--opts <opts> ...] [--pkgs <pkgs> ...] [--debug] [--optimize] [--reset]

OPTIONS:
  --version <version>     python version (default: 3.11.7)
  --config <config>       name of build config (default: static.max)
  --opts <opts>           configure options
  --pkgs <pkgs>           python packages to install
  --debug                 build debug python
  --optimize              optimize build
  --reset                 reset build
  -h, --help              Show help information.

```

A beginnings of a swift edition..





## Background

This project started from a concrete requirement in one [external project](https://github.com/shakfu/py-js) which led to the first python `builder`, this was then simplified into `pybuilder`, and finally producing `buildpy`, the latest iteration of this sequence.

After the python version of `buildpy` was created, it was thought a compiled version of the tool would be useful for bootstrapping purposes.

This led to a straightforward golang implementation, and then a rust implementation as well as haskell and swift implementations which are currently under development.

The idea is that all versions should eventually all be able to read, write and use a standard JSON build configuration file,



