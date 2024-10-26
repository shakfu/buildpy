use std::path::{Path, PathBuf};
use downloader::Downloader;
use crate::ops::log;
use crate::ops::process;

/// Download a file using wget
/// 
/// # Arguments
/// 
/// * `to_dir` - The directory to download the file to
/// * `url` - The URL to download the file from
pub fn wget(to_dir: PathBuf, url: &str) {
    log::info!("wget: {}", url);
    let dir = to_dir.to_str().unwrap();
    let args = vec!["-P", dir, url];
    process::cmd("wget", args, ".")
}

/// Download a file using curl
/// 
/// # Arguments
/// 
/// * `to_dir` - The directory to download the file to
/// * `url` - The URL to download the file from
pub fn curl(to_dir: PathBuf, url: &str) {
    log::info!("curl: {}", url);
    let dir = to_dir.to_str().unwrap();
    let args = vec!["-L", "--output-dir", dir, "-O", url];
    process::cmd("wget", args, ".")
}

/// Clone a git repository
/// 
/// # Arguments
/// 
/// * `url` - The URL to clone the repository from
/// * `branch` - The branch to clone
/// * `to_dir` - The directory to clone the repository to
/// * `recurse` - Whether to recurse submodules
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

/// Download a file
/// 
/// # Arguments
/// 
/// * `to_dir` - The directory to download the file to
/// * `url` - The URL to download the file from
pub fn download_file(to_dir: PathBuf, url: &str) {
    wget(to_dir, url);
}

/// Download a file using downloader
/// 
/// # Arguments
/// 
/// * `target` - The URL to download the file from
pub fn download_file2(target: &str) {
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
