# buildpy - a bunch of python3 builders

## Overview

This project provides different language implementations of a commandline tool which programmatically downloads, configures and builds python from source and also includes some additional features such as reducing build size and zipping the standard library.

Such customized builds can be useful for integration or embedding in other compiled projects.

The implementations so far are: python, golang, rust, c++, haskell, swift, c, and ocaml:

| python version    | python | c++    | golang | rust   | swift  | haskell |    c    | ocaml   |
| :---------------: | :----: | :----: | :----: | :----: | :----: | :----:  | :----:  | :----:  |
| 3.10              |    1   |    1   |    1   |    1   |    1   |    1    |    1    |    1    |
| 3.11              |    1   |    1   |    1   |    1   |    1   |    1    |    1    |    1    |
| 3.12              |    1   |    1   |    1   |    1   |    1   |    1    |    1    |    1    |
| 3.13              |    1   |    1   |    1   |    1   |    1   |    1    |    1    |    1    |
| 3.14              |    1   |    0   |    0   |    0   |    0   |    0    |    0    |    0    |


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


## Building and Usage

### Build All Implementations

To build all language variants at once:

```bash
make release
```

This creates optimized executables in the `bin/` directory:
- `buildpy.py` - Python script (48KB)
- `buildpy-go` - Go executable (5.1MB)
- `buildpy-rs` - Rust executable (2.6MB)
- `buildpy-cc` - C++ executable (535KB)
- `buildpy-c` - C executable (54KB)
- `buildpy-ml` - OCaml executable (1.7MB)
- `buildpy-sw` - Swift executable (2.0MB)
- `buildpy-hs` - Haskell executable (24.5MB)

### Build Individual Implementations

Each language implementation can be built independently:

```bash
# Python (reference implementation)
cd py/buildpy && python buildpy.py --help

# Go implementation
cd go/buildpy && make release

# Rust implementation
cd rs/buildpy && make release

# C++ implementation
cd cc/buildpy && make release

# C implementation
cd c/buildpy && make release

# OCaml implementation
cd ml/buildpy && make release

# Swift implementation
cd sw/buildpy && make release

# Haskell implementation
cd hs/buildpy && make release
```

### Common Usage Examples

All implementations support the same command-line interface:

```bash
# Show available configurations
./bin/buildpy-ml config

# Build Python with default settings
./bin/buildpy-go build

# Build specific Python version with custom config
./bin/buildpy-rs build -v 3.12.9 -c static_max

# Build with additional packages
./bin/buildpy-cc build -v 3.11.0 -c shared_max -p numpy,scipy

# Install only dependencies
./bin/buildpy-c deps

# Build with optimization and multiple jobs
./bin/buildpy-sw build -O -j 8
```


## Implementation Details

Feature coverage and notable aspects of `buildpy` variants by language:

| Features                   |  python | golang   | rust     | haskell  | c++      | swift    | c        | ocaml    |
| :------------------------- | :------:| :------: | :------: | :------: | :------: | :------: | :------: | :------: |
| Create Build Env           | x       | x        | x        | x        | x        | x        | x        | x        |
| Build Python Dependencies  | x       | x        | x        | x        | x        | x        | x        | x        |
| Configure Python Build     | x       | x        | x        | x        | x        | x        | x        | x        |
| Build/Install Python       | x       | x        | x        | x        | x        | x        | x        | x        |
| Clean Python Build         | x       | x        | x        | x        | x        | x        | x        | x        |
| Zip python library         | x       | x        | x        | x        | x        | x        | x        | x        |
| Configuration System       | x       | x        | x        | x        | x        | x        | x        | x        |
| Size of executable (macOS) | 48 Kb   | 5.1 MB   | 2.6 MB   | 24.5 MB  | 535 kb   | 2.0 MB   | 54 kb    | 1.7 MB   |
| Size of executable (linux) | 48 Kb   | 5.1 MB   | 6.3 MB   | 3.0 MB   | 484 Kb   | 3.9 MB   | 54 kb    | ~1.7 MB  |
| Tested on Linux            | x       | x        | x        | x        | x        | x        | x        | x        |
| Tested on macOS            | x       | x        | x        | x        | x        | x        | x        | x        |
| Tested on Windows          |         |          |          |          |          |          |          |          |


Use of External executables

| External Executable        |  python | golang   | rust     | haskell  | c++      | swift    | c        | ocaml    |
| :------------------------- | :------:| :------: | :------: | :------: | :------: | :------: | :------: | :------: |
| git                        |         | x        | x        | x        | x        |          | x        | x        |
| curl                       |         |          |          |          |          |          |          | x        |
| tar                        |         | x        | x        | x        | x        |          | x        | x        |
| zip                        |         | x        | x        | x        | x        |          | x        | x        |
| cmake                      |         | x        | x        | x        | x        |          |          | x        |
| make                       | x       | x        | x        | x        | x        | x        | x        | x        |
| bash                       | x       | x        | x        | x        | x        | x        | x        | x        |

The python implementation of `buildpy` uses the capabilities of its stdlib to download python and its dependencies, extract the results and uses external executables when calling `./configure` and `make`.

In most cases, if `tar` and `wget` are not available, then `git` suffices to download source code.

Note that it was not a goal to make the non-python versions to be similarly 'self-contained' as the python version, which relies on the python stdlib, and reduce the need for external executables. This may change in the future if it is not too much work.

## Language Implementation Details

### Python Implementation (`py/buildpy/`)
**Reference implementation - 48KB executable**
- **Self-contained**: Uses Python stdlib for downloads, extraction, and file operations
- **Minimal dependencies**: Only requires `make` and `bash` for build operations
- **Original design**: The foundational implementation that defines the architecture
- **JSON configuration**: Can read/write standardized build configurations
- **Cross-platform**: Works on macOS, Linux with native Python features

### Go Implementation (`go/buildpy/`)
**Production-ready - 5.1MB executable**
- **Concurrent builds**: Parallel dependency installation using goroutines
- **CLI framework**: Uses Cobra for sophisticated command-line interface
- **JSON/YAML support**: Configuration serialization and persistence
- **Error handling**: Comprehensive error management and logging
- **Cross-compilation**: Easy building for multiple platforms

### Rust Implementation (`rs/buildpy/`)
**Memory-safe - 2.6MB executable**
- **Zero-cost abstractions**: High performance with safety guarantees
- **Cargo ecosystem**: Leverages Rust's package manager and build system
- **Async support**: Non-blocking I/O for network operations
- **Type safety**: Compile-time guarantees prevent runtime errors
- **Cross-platform**: Single codebase for all supported platforms

### C++ Implementation (`cc/buildpy/`)
**Modern C++ - 535KB executable**
- **C++17 features**: Modern standard library and language features
- **CMake build**: Industry-standard build system integration
- **STL utilization**: Standard library containers and algorithms
- **Template metaprogramming**: Compile-time optimizations
- **Platform abstraction**: Unified interface across operating systems

### C Implementation (`c/buildpy/`)
**Minimal footprint - 54KB executable**
- **C11 standard**: Modern C with safety features and portability
- **Zero dependencies**: Completely self-contained implementation
- **Memory management**: Manual but safe memory handling patterns
- **System integration**: Direct system calls for optimal performance
- **Embedded-friendly**: Suitable for resource-constrained environments

### OCaml Implementation (`ml/buildpy/`)
**Functional approach - 1.7MB executable**
- **Functional programming**: Immutable data structures and pure functions
- **Pattern matching**: Elegant handling of configuration variants
- **Type inference**: Strong static typing without explicit annotations
- **Module system**: Clean separation of concerns and code organization
- **Dune build system**: Modern OCaml build toolchain integration
- **Error handling**: Algebraic data types for safe error management

### Haskell Implementation (`hs/buildpy/`)
**Pure functional - 24.5MB executable**
- **Lazy evaluation**: Computations performed only when needed
- **Monadic I/O**: Pure functional approach to side effects
- **Type classes**: Generic programming with strong type guarantees
- **Stack/Cabal**: Haskell ecosystem build tools
- **Concurrent**: Software Transactional Memory for safe concurrency
- **Version support**: Full support for Python 3.10-3.13 with optimized configurations for 3.11-3.12

### Swift Implementation (`sw/buildpy/`)
**Apple ecosystem - 2.0MB executable**
- **Swift Package Manager**: Native dependency management
- **Protocol-oriented**: Interface-based design patterns
- **ARC memory management**: Automatic reference counting
- **Cross-platform**: Works beyond Apple platforms with Swift on Linux
- **Safety features**: Optionals and strong typing prevent common errors

## Common Architecture

All implementations follow the same modular architecture:

### Core Components

1. **Configuration System**
   - Python version support (3.10-3.13)
   - Build variants (static_max, static_mid, static_min, shared_max, shared_mid)
   - Platform-specific optimizations (Darwin/Linux)
   - Module categorization (core, static, shared, disabled)

2. **Shell Command Layer**
   - Command execution with error handling
   - Working directory management
   - Environment variable support
   - Platform-specific command variations

3. **Project Management**
   - Build environment setup (`./build/{downloads,src,install}/`)
   - Directory creation and cleanup
   - Reset and clean operations
   - Path management and validation

4. **Dependency Builders**
   - **OpenSSL Builder**: SSL/TLS library (1.1.1w)
   - **Bzip2 Builder**: Compression library (1.0.8)
   - **XZ Builder**: LZMA compression (5.6.3)
   - Parallel dependency installation where supported

5. **Python Builder**
   - Source download (python.org or GitHub)
   - Configure script execution
   - Custom Setup.local generation
   - Build and installation process
   - Post-build cleanup and optimization

6. **Command Line Interface**
   - Consistent commands across all implementations:
     - `build` - Full Python build process
     - `config` - Show configuration details
     - `deps` - Install dependencies only
   - Common flags and options
   - Help and usage information

### Build Process Flow

```
1. Setup → Create build directories
2. Dependencies → Build OpenSSL, Bzip2, XZ
3. Download → Get Python source code
4. Configure → Generate Setup.local and run ./configure
5. Build → Compile Python with make
6. Install → Install to build/install/python
7. Cleanup → Remove unnecessary files and zip stdlib
```

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

This led to a straightforward golang implementation, and then a rust implementation as well as haskell, c++, swift, c, and ocaml implementations. Each implementation showcases different programming paradigms:

- **Python**: Dynamic, batteries-included, and rapid prototyping with extensive standard library
- **Go**: Concurrent, simple, and fast development
- **Rust**: Memory safety with zero-cost abstractions
- **C++**: Modern systems programming with STL
- **C**: Minimal footprint with maximum portability
- **OCaml**: Functional programming with strong type inference
- **Swift**: Safe, expressive, and protocol-oriented
- **Haskell**: Pure functional with lazy evaluation

The Python implementation serves as the reference design, leveraging Python's philosophy of "batteries included" and readable code.

The idea is that all versions should eventually all be able to read, write and use a standard JSON build configuration file, providing users with multiple implementation choices based on their specific requirements, from minimal embedded systems (C) to high-level functional programming (OCaml/Haskell).
