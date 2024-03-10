// use std::path::PathBuf;
use crate::ops::log;
use crate::ops::process;

pub fn makedirs(path: &str) {
    log::info!("makedirs {path}");
    match std::fs::create_dir_all(std::path::Path::new(path)) {
        Ok(_) => log::info!("success: directory created"),
        Err(err) => {
            log::error!("failure: {}", err);
            std::process::exit(1)
        }
    }
}




// func CmakeInstall(builddir string, prefix string) {
//     var args = []string{"--install", builddir, "--prefix", prefix}
//     log.Info("CmakeInstall", "exe", "cmake", "args", args)
//     Cmd(".", "cmake", args...)
// }

pub fn cmake_configure(src_dir: &str, build_dir: &str, opts: Vec<&str>) {
    let mut args = vec!["S", src_dir, "-B", build_dir];
    args.extend(opts);    
    process::cmd("cmake", args, ".");
}

pub fn cmake_build(build_dir: &str, release: bool) {
    let mut args = vec!["--build", build_dir];
    if release {
        args.extend(vec!["--config", "Release"]);
    }
    process::cmd("cmake", args, ".");
}

pub fn cmake_install(build_dir: &str, prefix: &str) {
    let args = vec!["--install", build_dir, "--prefix", prefix];
    process::cmd("cmake", args, ".");
}