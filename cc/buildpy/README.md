# buildpy (c++)

A minimal c++ implementation

## Usage

```bash
$ ./buildpy --help
Usage: buildpy [--help] [--version] [--pyversion VAR] [--config VAR] [--optimize] [--usegit]

Optional arguments:
  -h, --help       shows help message and exits 
  -v, --version    prints version information and exits 
  -p, --pyversion  python version [default: "3.12.2"]
  -c, --config     build configuration name [default: "static_max"]
  -o, --optimize   optimize python build 
  -g, --usegit     download using git 
```


## Libraries

- [fmt](https://github.com/fmtlib/fmt)
- [argparse](https://github.com/p-ranav/argparse)
- [glob](https://github.com/p-ranav/glob)
- [logy](https://github.com/squillero/logy)

maybe used later

- [indicators](https://github.com/p-ranav/indicators)
- [taskflow](https://github.com/taskflow/taskflow)
- [tiny-process-library](https://gitlab.com/eidheim/tiny-process-library)
