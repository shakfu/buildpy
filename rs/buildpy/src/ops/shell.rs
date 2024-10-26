/// Shell operations
/// 
/// # Examples
/// 
/// ```
/// use crate::ops::shell;
/// shell::recursive_remove(Path::new("/path/to/dir"), vec!["*.txt", "*.log"]);
/// ```

use crate::ops::log;
use crate::ops::process;
use std::collections::HashMap;
use std::path::Path;
use walkdir::WalkDir;
use globset::{Glob, GlobSetBuilder};

/// Recursively remove files in a directory that match the given patterns
/// 
/// # Arguments
/// 
/// * `dir` - The directory to recursively remove files from
/// * `patterns` - The patterns to match against the files
pub fn recursive_remove(dir: &Path, patterns: Vec<String>) {
    // setup globset
    let mut builder = GlobSetBuilder::new();
    for pat in patterns {
        builder.add(Glob::new(&pat).unwrap());
    }
    let set = builder.build().unwrap();
    for entry in WalkDir::new(dir) {
        let entry = entry.unwrap();
        if set.is_match(entry.path().file_name().unwrap()) {
            if entry.file_type().is_file() {
                log::info!("removing file {}", entry.path().display());
                std::fs::remove_file(entry.path()).unwrap();
            } else {
                log::info!("removing dir {}", entry.path().display());
                std::fs::remove_dir_all(entry.path()).unwrap();
            }
        }
    }
}

/// Make directories
/// 
/// # Arguments
/// 
/// * `path` - The path to the directory to make
pub fn makedirs(path: &str) {
    log::info!("makedirs {path}");
    match std::fs::create_dir_all(std::path::Path::new(path)) {
        Ok(_) => log::info!("success: directory created"),
        Err(err) => {
            log::error!("failure: {}", err);
            std::process::exit(1);
        }
    }
}

/// Move a file or directory
/// 
/// # Arguments
/// 
/// * `src` - The source path
/// * `dst` - The destination path
pub fn mv(src: std::path::PathBuf, dst: std::path::PathBuf) {
    let _ = std::fs::rename(src, dst);
}

/// Configure a CMake project
/// 
/// # Arguments
/// 
/// * `src_dir` - The source directory
/// * `build_dir` - The build directory
/// * `opts` - The options to pass to CMake
pub fn cmake_configure(src_dir: &str, build_dir: &str, opts: Vec<&str>) {
    let mut args = vec!["-S", src_dir, "-B", build_dir];
    args.extend(opts);
    process::cmd("cmake", args, ".");
}

/// Configure a CMake project with environment variables
/// 
/// # Arguments
/// 
/// * `src_dir` - The source directory
/// * `build_dir` - The build directory
/// * `opts` - The options to pass to CMake
/// * `envs` - The environment variables to pass to CMake
pub fn cmake_configure_env(src_dir: &str, build_dir: &str, opts: Vec<&str>, envs: HashMap<String, String>) {
    let mut args = vec!["-S", src_dir, "-B", build_dir];
    args.extend(opts);
    process::cmd_env("cmake", args, ".", envs);
}

/// Build a CMake project
/// 
/// # Arguments
/// 
/// * `build_dir` - The build directory
/// * `release` - Whether to build in release mode
pub fn cmake_build(build_dir: &str, release: bool) {
    let mut args = vec!["--build", build_dir];
    if release {
        args.extend(vec!["--config", "Release"]);
    }
    process::cmd("cmake", args, ".");
}

/// Build a CMake project with environment variables
/// 
/// # Arguments
/// 
/// * `build_dir` - The build directory
/// * `release` - Whether to build in release mode
/// * `envs` - The environment variables to pass to CMake
pub fn cmake_build_env(build_dir: &str, release: bool, envs: HashMap<String, String>) {
    let mut args = vec!["--build", build_dir];
    if release {
        args.extend(vec!["--config", "Release"]);
    }
    process::cmd_env("cmake", args, ".", envs);
}

/// Install a CMake project
/// 
/// # Arguments
/// 
/// * `build_dir` - The build directory
/// * `prefix` - The prefix to install to
pub fn cmake_install(build_dir: &str, prefix: &str) {
    let args = vec!["--install", build_dir, "--prefix", prefix];
    process::cmd("cmake", args, ".");
}

/// Untar an archive
/// 
/// # Arguments
/// 
/// * `archive` - The archive to untar
/// * `srcdir` - The source directory to untar to
pub fn untar(archive: &str, srcdir: &str) {
    let args = vec!["xvf", archive, "-C", srcdir];
    process::cmd("tar", args, ".");
}

/// Zip a directory
/// 
/// # Arguments
/// 
/// * `zip_path` - The path to the zip file
/// * `libpath` - The path to the directory to zip
pub fn zipfile(zip_path: &str, libpath: &str) {
    let args = vec!["-r", zip_path, "."];
    process::cmd("zip", args, libpath);
}

/// Change the permissions of a file or directory
/// 
/// # Arguments
/// 
/// * `target` - The target to change the permissions of
/// * `mode` - The mode to change the permissions to
/// * `cwd` - The current working directory
pub fn chmod(target: &str, mode: u32, cwd: &str) {
    let perms = mode.to_string();
    let args = vec![perms.as_str(), target];
    process::cmd("chmod", args, cwd)

}