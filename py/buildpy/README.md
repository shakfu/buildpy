# buildpy - single-file python builder

Currently supporting only building python 3.11 or 3.12

Lightest-weight single script/module, a 'modern' rewrite with the simplest interface given collective experience of `builder` and `pybuilder`. The future, so to speak.

Only handles python 3.11 - 3.12 and has a builtin configuration system which handles differences between build variants efficiently.

## Usage

```bash
% python3 buildpy.py --help
usage: buildpy.py [-h] [--debug] [--version VERSION] [--config NAME] [--reset]
                  [--optimize] [--pkgs PKG [PKG ...]]

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
```

or just `make` to build the default configuration.


## Gotchas

- if `libb2` is installed on your system, `_blake2` will be linked to it creating a local dependency (need test for this)

## Configurations

A configuration has a name with the structure `<build-type>.<size-type>`. For example, a `static.max` configuration means a `static` build-type of the `max` size-type, in other words, a build variant where libpython is statically linked and which tries to include the maximum number of extensions outside of the default configuration.

The following configuration are implemented:

### static

```python
config_options = [
	"--disable-test-modules",
	"--without-ensurepip",
]

# where

static.max 		 # missing ctypes
static.mid = static.max - ["_ssl", "_hashlib"]
static.min
static.bootstrap # absolute minimum based on Setup.bootstrap
```

### shared

```python
config_options = [
	"--disable-test-modules",
	"--without-ensurepip",
	"--without-static-libpython",
] + ["--enable-shared"]

# where

shared.max
shared.mid = shared.max - ["_ctypes", "_ssl", "_hashlib", "_decimal"]
shared.min
```

### framework

macos only (not yet implemented)

```
framework.max
framework.mid
framework.min
```


### buildpy (python)



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

