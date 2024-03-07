# buildpy - rust edition

Approach is to use a number of mature crates from rust's rich ecosystem


## Crate Dependencies

```toml
[dependencies]
command-run = "1.1.1"
downloader = "0.2.7"
flate2 = "1.0.28"
indicatif = "0.17.8"
simplelog = { version = "^0.12.2", features = ["paris"] }
xshell = "0.2.5"
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.9"
```

## clap - commandline parsing

- [clap](https://crates.io/crates/clap)


## indicatif - progressbar

- [indicatif](https://crates.io/crates/indicatif)
- [indicatif-log-bridge](https://crates.io/crates/indicatif-log-bridge)

```rust
let logger =
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .build();
let multi = MultiProgress::new();

LogWrapper::new(multi.clone(), logger)
    .try_init()
    .unwrap();

let pg = multi.add(ProgressBar::new(10));
for i in (0..10) {
    std::thread::sleep(Duration::from_micros(100));
    info!("iteration {}", i);
    pg.inc(1);
}
pg.finish();
multi.remove(&pg);
```


## simplelog - logging

- [simplelog](https://crates.io/crates/simplelog)

```rust
#[macro_use] extern crate log;
extern crate simplelog;

use simplelog::*;

use std::fs::File;

fn main() {
    CombinedLogger::init(
        vec![
            TermLogger::new(LevelFilter::Warn, Config::default(), TerminalMode::Mixed, ColorChoice::Auto),
            WriteLogger::new(LevelFilter::Info, Config::default(), File::create("my_rust_binary.log").unwrap()),
        ]
    ).unwrap();

    error!("Bright red error");
    info!("This only appears in the log file");
    debug!("This level is currently not enabled for any logger");
}
```



## command-run - process execution

- [command-run](https://crates.io/crates/command-run)

```rust
// This will return an error if the command did not exit successfully
// (controlled with the `check` field).
let output = Command::with_args("echo", &["hello", "world"])
    .enable_capture()
    .run()?;
assert_eq!(output.stdout_string_lossy(), "hello world\n");
```



## flate2 - compression/decompression

- [flate2](https://crates.io/crates/flate2)

```rust
// compression

use std::io::prelude::*;
use flate2::Compression;
use flate2::write::ZlibEncoder;

fn main() {
    let mut e = ZlibEncoder::new(Vec::new(), Compression::default());
    e.write_all(b"foo");
    e.write_all(b"bar");
    let compressed_bytes = e.finish();
}
```


```rust
// decompression
use std::io::prelude::*;
use flate2::read::GzDecoder;

fn main() {
    let mut d = GzDecoder::new("...".as_bytes());
    let mut s = String::new();
    d.read_to_string(&mut s).unwrap();
    println!("{}", s);
}
```

### downloader - downloading

- [downloader](https://crates.io/crates/downloader)

```rust
// Setup warnings/errors:
#![forbid(unsafe_code)]
#![deny(bare_trait_objects, unused_doc_comments, unused_import_braces)]
// Clippy:
#![warn(clippy::all, clippy::nursery, clippy::pedantic)]
#![allow(clippy::non_ascii_literal)]

use downloader::Downloader;

// Run example with: cargo run --example tui_basic --features tui
fn main() {
    let mut downloader = Downloader::builder()
        .download_folder(std::path::Path::new("/tmp"))
        .parallel_requests(1)
        .build()
        .unwrap();

    // Download with an explicit filename
    let dl = downloader::Download::new("https://example.org/")
        .file_name(std::path::Path::new("example.html"));

    // Download with an inferred filename
    let dl2 = downloader::Download::new(
        "https://cdimage.debian.org/debian-cd/current/i386/iso-cd/debian-10.9.0-i386-netinst.iso",
    );

    let result = downloader.download(&[dl, dl2]).unwrap();

    for r in result {
        match r {
            Err(e) => print!("Error occurred! {}", e.to_string()),
            Ok(s) => print!("Success: {}", &s),
        };
    }
}
```


### xshell - shell scripting

- [xshell](https://crates.io/crates/xshell)

```rust
//! Clones a git repository and publishes it to crates.io.
use xshell::{cmd, Shell};

fn main() -> anyhow::Result<()> {
    let sh = Shell::new()?;

    let user = "matklad";
    let repo = "xshell";
    cmd!(sh, "git clone https://github.com/{user}/{repo}.git").run()?;
    sh.change_dir(repo);

    let test_args = ["-Zunstable-options", "--report-time"];
    cmd!(sh, "cargo test -- {test_args...}").run()?;

    let manifest = sh.read_file("Cargo.toml")?;
    let version = manifest
        .split_once("version = \"")
        .and_then(|it| it.1.split_once('\"'))
        .map(|it| it.0)
        .ok_or_else(|| anyhow::format_err!("can't find version field in the manifest"))?;

    cmd!(sh, "git tag {version}").run()?;

    let dry_run = if sh.var("CI").is_ok() { None } else { Some("--dry-run") };
    cmd!(sh, "cargo publish {dry_run...}").run()?;

    Ok(())
}
```

## serde - serialization/deserialization

- [serde](https://crates.io/crates/serde)
- [serde_yaml](https://crates.io/crates/serde_yaml)

```rust
use std::collections::BTreeMap;

fn main() -> Result<(), serde_yaml::Error> {
    // You have some type.
    let mut map = BTreeMap::new();
    map.insert("x".to_string(), 1.0);
    map.insert("y".to_string(), 2.0);

    // Serialize it to a YAML string.
    let yaml = serde_yaml::to_string(&map)?;
    assert_eq!(yaml, "x: 1.0\ny: 2.0\n");

    // Deserialize it back to a Rust type.
    let deserialized_map: BTreeMap<String, f64> = serde_yaml::from_str(&yaml)?;
    assert_eq!(map, deserialized_map);
    Ok(())
}
```