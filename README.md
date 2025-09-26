# buildpy - a bunch of python3 builders

## Overview

This project provides different language implementations of a commandline tool which programmatically downloads, configures and builds python from source and also includes some additional features such as reducing build size and zipping the standard library.

Such customized builds can be useful for integration or embedding in other compiled projects.

The implementations so far are: python, golang, rust, c++, haskell (wip), swift, and c:

| python version    | python | c++    | golang | rust   | swift  | haskell |    c    |
| :---------------: | :----: | :----: | :----: | :----: | :----: | :----:  | :----:  |
| 3.10              |    1   |    1   |    1   |    1   |    1   |    0    |    1    |
| 3.11              |    1   |    1   |    1   |    1   |    1   |    0    |    1    |
| 3.12              |    1   |    1   |    1   |    1   |    1   |    0    |    1    |
| 3.13              |    1   |    1   |    1   |    1   |    1   |    0    |    1    |


The design for the tool, called `buildpy`, was initially conceived, prototyped and implemented in python3 as an improvement on some earlier incarnations (see Background section below) which are also included in this project. It provides for the following general steps to build python from source:

1. Create local build environment / project which consists of the following folder structure

```bash
./build/
./build/downloads/
./build/src/
./build/install/
```

2. Download, build, python dependencies {openssl, bzip2, xz, ..} and install into `build/install/<name>`.

3. Download the source code of a particular version of python3 from python.org or from github

4. Configure python3 build using `configure` options and custom `Setup.local` file

5. Build python3 and install into `build/install/python`

6. Clean or remove extraneous libraries, extensions, tests, modules

7. Zip standard library


## Overview

To build all variants type `make release` in the root of this project. This will build all executables and place them in the `bin` directory.

```bash
make release
```

To build a specific case, type the same in the root of the subproject.


## Implementation Details

Feature coverage and notable aspects of `buildpy` variants by language:

| Features                   |  python | golang   | rust     | haskell  | c++      | swift    | c        |
| :------------------------- | :------:| :------: | :------: | :------: | :------: | :------: | :------: |
| Create Build Env           | x       | x        | x        | x        | x        | x        | x        |
| Build Python Dependencies  | x       | x        | x        | x        | x        | x        | x        |
| Configure Python Build     | x       | x        | x        | x        | x        | x        | x        |
| Build/Install Python       | x       | x        | x        | x        | x        | x        | x        |
| Clean Python Build         | x       | x        | x        |          | x        | x        | x        |
| Zip python library         | x       | x        | x        |          | x        | x        | x        |
| Configuration System       | x       | x        | x        |          | x        | x        | x        |
| Size of executable (macOS) | 48 Kb   | 5.1 MB   | 2.6 MB   | 24.5 MB  | 535 kb   | 2.0 MB   | 54 kb    |
| Size of executable (linux) | 48 Kb   | 5.1 MB   | 6.3 MB   | 3.0 MB   | 484 Kb   | 3.9 MB   | 54 kb    |
| Tested on Linux            | x       | x        | x        | x        | x        | x        | x        |
| Tested on macOS            | x       | x        | x        | x        | x        | x        | x        |
| Tested on Windows          |         |          |          |          |          |          |          |


Use of External executables

| External Executable        |  python | golang   | rust     | haskell  | c++      | swift    | c        |
| :------------------------- | :------:| :------: | :------: | :------: | :------: | :------: | :------: |
| git                        |         | x        | x        | x        | x        |          | x        |
| wget                       |         | x        | x        | x        | x        |          | x        |
| tar                        |         | x        | x        | x        | x        |          | x        |
| zip                        |         | x        | x        | x        | x        |          | x        |
| cmake                      |         | x        | x        | x        | x        |          |          |
| make                       | x       | x        | x        | x        | x        | x        | x        |
| bash                       | x       | x        | x        | x        | x        | x        | x        |

The python implementation of `buildpy` uses the capabilities of its stdlib to download python and its dependencies, extract the results and uses external executables when calling `./configure` and `make`.

In most cases, if `tar` and `wget` are not available, then `git` suffices to download source code.

Note that it was not a goal to make the non-python versions to be similarly 'self-contained' as the python version, which relies on the python stdlib, and reduce the need for external executables. This may change in the future if it is not too much work.

## C Implementation Features

The C implementation includes several notable enhancements:

- **Advanced Configuration System**: C11+ based configuration system that replicates the Go version functionality with comprehensive Python module management
- **Version-Specific Logic**: Full support for Python 3.10-3.13 with version-specific module configurations and platform adaptations
- **Setup.local Generation**: Automated generation of Python's Setup.local configuration file with proper module categorization
- **Platform Optimization**: Platform-specific optimizations for macOS and Linux builds including static linking strategies
- **Minimal Dependencies**: Compact 54kb executable with C11 standard compliance and cross-platform portability
- **Memory Safety**: Comprehensive bounds checking and memory management throughout the codebase
- **Debug Support**: Extensive debugging capabilities with module set analysis and configuration validation




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

The product is a python installation with the following name:

`py-<buildtype>-<sizetype>-<pyversion>-<platform>-<arch>`


`py-static-max-3.12.2-darwin-x86_64`


`py-3.12.2-static-max-darwin-x86_64`

a symlink directory `<python>` will be created and link to above.


## Background

This project started from a concrete requirement in one [external project](https://github.com/shakfu/py-js) which led to the first python `builder`, this was then simplified into `pybuilder`, and finally producing `buildpy`, the latest iteration of this sequence.

After the python version of `buildpy` was created, it was thought a compiled version of the tool would be useful for bootstrapping purposes.

This led to a straightforward golang implementation, and then a rust implementation as well as haskell, c++, swift, and c implementations. The C implementation provides equivalent functionality to the C++ version while maintaining portability and minimal resource usage.

The idea is that all versions should eventually all be able to read, write and use a standard JSON build configuration file,



