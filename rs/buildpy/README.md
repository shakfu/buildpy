# buildpy - rust edition

Approach is to use a number of mature crates from rust's rich ecosystem


```bash
% ./buildpy --help
Builds python from source

Usage: buildpy [OPTIONS]

Options:
  -p, --pyversion <PYVERSION>  Python version [default: 3.11.8]
  -c, --config <CONFIG>        Config name [default: static_max]
      --opts <OPTS>            Config options
      --pkgs <PKGS>            Install python packages
  -o, --optimize               Optimize build
  -g, --git                    Use git to download code
  -r, --reset                  reset before build
  -d, --demo                   Run Demo
  -j, --jobs <JOBS>            Parallel build jobs [default: 4]
  -h, --help                   Print help
  -V, --version                Print version
sa@minx buildpy %
```

A rust version of `buildpy`.

Mostly mirrors the python version except for the following:

- Round-trip serialization/deserialization from/to json

