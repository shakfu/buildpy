// use std::path::PathBuf;
use std::collections::HashMap;
use crate::ops::log;
use crate::ops::process;

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

pub fn mv(src: &str, dst: &str) {
    std::fs::rename(src, dst).expect(&format!("failed to move {} to {}", src, dst));
}

pub fn cmake_configure(src_dir: &str, build_dir: &str, opts: Vec<&str>) {
    let mut args = vec!["S", src_dir, "-B", build_dir];
    args.extend(opts);
    process::cmd("cmake", args, ".");
}

pub fn cmake_configure_env(src_dir: &str, build_dir: &str, opts: Vec<&str>, envs: HashMap<String, String>) {
    let mut args = vec!["S", src_dir, "-B", build_dir];
    args.extend(opts);
    process::cmd_env("cmake", args, ".", envs);
}

pub fn cmake_build(build_dir: &str, release: bool) {
    let mut args = vec!["--build", build_dir];
    if release {
        args.extend(vec!["--config", "Release"]);
    }
    process::cmd("cmake", args, ".");
}

pub fn cmake_build_env(build_dir: &str, release: bool, envs: HashMap<String, String>) {
    let mut args = vec!["--build", build_dir];
    if release {
        args.extend(vec!["--config", "Release"]);
    }
    process::cmd_env("cmake", args, ".", envs);
}

pub fn cmake_install(build_dir: &str, prefix: &str) {
    let args = vec!["--install", build_dir, "--prefix", prefix];
    process::cmd("cmake", args, ".");
}

pub fn untar(archive: &str, srcdir: &str) {
    let args = vec!["xvf", archive, "-C", srcdir];
    process::cmd("tar", args, ".");
}

pub fn zipfile(zip_path: &str, libpath: &str) {
    let args = vec!["-r", zip_path, "."];
    process::cmd("zip", args, libpath);
}
