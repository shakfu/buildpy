# pybuild

A python3 script to build python3 from source (currently for macos)

## Features

- Downloads the python source from python.org and builds it according to 'static', 'shared', and 'framework' variations for macos.

- Provides an option to patch the python source to specialize build

It should be straightforward for windows builds as well ultimately.

## Usage

```
$ python pybuild.py --help
usage: pybuild.py [-h] [-v] {framework,shared,static} ...

pybuild: builds python from src

optional arguments:
  -h, --help            show this help message and exit
  -v, --version         show program's version number and exit

subcommands:
  valid subcommands

  {framework,shared,static}
                        additional help
    framework           build framework python
    shared              build shared python
    static              build static python

```

and some options in each case:

```
$ python pybuild.py static --help
usage: pybuild.py static [-h] [-d] [-r] [-i] [-b] [-c] [-z]

optional arguments:
  -h, --help      show this help message and exit
  -d, --download  download python build/downloads
  -r, --reset     reset python build
  -i, --install   install python to build/lib
  -b, --build     build python in build/src
  -c, --clean     clean python in build/src
  -z, --ziplib    zip python library
```

## What it does

1. Downloads the required archives into `build/downloads` 

2. Builds the variation in `build/src`

3. Installs the library to `build/lib`


```bash
$ tree -L 3
.
├── LICENSE
├── README.md
├── build
│   ├── downloads
│   │   ├── Python-3.9.1.tgz
│   │   ├── bzip2-1.0.8.tgz
│   │   ├── openssl-1.1.1g.tgz
│   │   └── xz-5.2.5.tgz
│   ├── lib
│   │   ├── Python.framework
│   │   ├── bzip2
│   │   ├── openssl
│   │   ├── python-shared
│   │   ├── python-static
│   │   └── xz
│   └── src
│       ├── Python-3.9.1
│       ├── bzip2-1.0.8
│       ├── openssl-1.1.1g
│       └── xz-5.2.5
└── pybuild.py
```

It also has a patch folder for optional patches to create specialized builds (minimal builds for example).
