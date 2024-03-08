# CHANGELOG

All notable project-wide changes will be documented in this file. Note that each subproject has its own CHANGELOG.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and [Commons Changelog](https://common-changelog.org). This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Types of Changes

- Added: for new features.
- Changed: for changes in existing functionality.
- Deprecated: for soon-to-be removed features.
- Removed: for now removed features.
- Fixed: for any bug fixes.
- Security: in case of vulnerabilities.

---

## [0.0.1]

- Adde `write_json` options

- Added `make_relocatable` feature for `framework-*` build variants on macOS to make the installed prefix relocatable

- Added `framework` build-type for macOS

- Added `make_relocatable` method for `shared-*` build variants on macOS to make the installed prefix relocatable

- Added cmdline option to add config options `--cfg-opts`

- Changed `cmd` from `subprocess.call` to `subprocess.check_call` which fails on error

- Added Linux support (Python versions 3.11.7 and 3.12.2 tested).

- Added recursive `glob_remove` method for subtractive reduction of the build.

- Added static and shared `simple` and `pybind11` examples.

- Added flexible in-module configuration system.

