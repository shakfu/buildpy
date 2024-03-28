# buildpy (golang edition)

An ongoing translation of the python `buildpy.py` into golang

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


## Dev Notes

### Tool Dependencies

At first, I though I could code everything in go and just use the standard library, but I opted to use check if thirdparty executables are avalable on the system and use them if they are found:

```golang
"git" 				// downloading code from git repos
"wget", "curl", 	// downloading archives from releases
"tar",  "zip", 		// uncompress archive, zip stdlib
"cmake", "make", 	// build systems
"bash", 			// used for `./configure`
"install_name_tool" // macos rpath modifier
"patchelf" 			// linux rpath modifier
```

This can be reduced to the essentials

```golang
"git", 				// downloading code
"zip", 				// zip stdlib
"cmake", "make", 	// build systems
"bash", 			// used for `./configure`
```

### Interesting golang Packages

- [archiver](https://github.com/mholt/archiver/) to maybe address the compression

- [sliver](https://github.com/leaanthony/slicer) slice mgmt

