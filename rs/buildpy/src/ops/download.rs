use std::path::{Path, PathBuf};

use downloader::Downloader;

use crate::ops::log;
use crate::ops::process;

pub fn download_file(target: &str) {
    log::info!("downloading {}", target);
    let mut downloader = Downloader::builder()
        .download_folder(std::path::Path::new("/tmp"))
        .parallel_requests(1)
        .build()
        .unwrap();

    let d1 = downloader::Download::new(target);

    let result = downloader.download(&[d1]).unwrap();

    for r in result {
        match r {
            Err(e) => log::error!("Error occurred! {}", e),
            Ok(s) => log::info!("Success: {}", &s),
        };
    }
}

pub fn git_clone(url: &str, branch: &str, to_dir: PathBuf, recurse: bool) {
    let mut args = vec!["clone", url, "-b", branch, "--depth=1"];
    if let Some(stem) = Path::new(url).file_stem() {
        if let Some(target) = to_dir.join(stem).into_os_string().to_str() {
            if recurse {
                args.extend_from_slice(&["--recurse-submodules", "--shallow-submodules", target])
            } else {
                args.push(target)
            }
            process::cmd("git", args, ".");
        }
    }
}
