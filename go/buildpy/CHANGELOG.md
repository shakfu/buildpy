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

- Added yaml serialization / deserialization of `config.Config` types

- Added improved configuration cli options

- Added multi-config write

- Added `config.Config.Write` to `Setup.local` formal

- Added `shell.RecursiveRemove` for post-install cleanup of python build

- Added improved structured logging

- Added builders for python dependencies: `openssl`, `bzip2`, and `lzma`

- Added `shell.GitClone` to download src via `git` for all deps and including python.

- Added preliminary golang implementation of python `buildpy.py` script