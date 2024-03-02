# buildpy (golang edition)

An ongoing translation of the python `buildpy.py` into golang


## Dev Notes

### Tool Dependencies

At first, I though I could code everything in go and just use the standard library, but I opted to use thirdparty executables instead:

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

- [copier](https://github.com/jinzhu/copier) for flexible object copying

