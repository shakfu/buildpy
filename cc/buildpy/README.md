# buildpy (c++)

A minimal c++ implementation for building Python from source.

## Overview

This module provides functionality to automate the process of building
Python from source code. It includes classes and utilities for:

- Downloading and extracting Python source code
- Configuring build options
- Compiling Python and its dependencies
- Installing the built Python distribution

The main class, PythonBuilder, orchestrates the entire build process,
while supporting classes like OpenSSLBuilder handle specific dependencies.

Key features:

- Supports multiple Python versions (3.11.x to 3.13.x)
- Configurable build options (static/shared, optimizations)
- Handles dependency management (OpenSSL, etc.)
- Cross-platform support (Linux, macOS)

## Usage

```bash
$ ./buildpy --help
Usage: buildpy [--help] [--version] [--pyversion VAR] [--config VAR] [--optimize] [--git] [--reset]

Optional arguments:
  -h, --help       shows help message and exits 
  -v, --version    prints version information and exits 
  -p, --pyversion  python version [default: "3.11.8"]
  -c, --config     build configuration name [default: "static_max"]
  -o, --optimize   optimize python build 
  -g, --git        download using git 
  -r, --reset      reset build environment
```

## Libraries

- [fmt](https://github.com/fmtlib/fmt)
- [argparse](https://github.com/p-ranav/argparse)
- [glob](https://github.com/p-ranav/glob)
- [logy](https://github.com/squillero/logy)

maybe used later

- [indicators](https://github.com/p-ranav/indicators)
- [thread-pool](https://github.com/bshoshany/thread-pool)
- [taskflow](https://github.com/taskflow/taskflow)
- [tiny-process-library](https://gitlab.com/eidheim/tiny-process-library)
