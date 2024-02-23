# buildpy - single-file python builder

## Usage

```bash

make

```

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