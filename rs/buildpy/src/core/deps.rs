use std::path::{Path, PathBuf};

use super::Project;
use crate::ops::process;

pub struct Dependency {
    pub name: String,
    pub version: String,
    pub download_url: String,
    pub repo_url: String,
    pub repo_branch: String,
    pub config_options: Vec<String>,
    pub staticlibs: Vec<String>,
    pub use_git: bool,
    pub parallel: i16, // n workers
    pub duration: i16, // seconds
    pub project: Project,
}

impl Dependency {
    pub fn new(
        name: &str,
        version: &str,
        download_url: &str,
        repo_url: &str,
        repo_branch: &str,
        config_options: Vec<String>,
        staticlibs: Vec<String>,
    ) -> Self {
        Self {
            name: name.to_string(),
            version: version.to_string(),
            download_url: download_url.to_string(),
            repo_url: repo_url.to_string(),
            repo_branch: repo_branch.to_string(),
            config_options,
            staticlibs,
            use_git: true,
            parallel: 4,
            duration: 0,
            project: Project::new(),
        }
    }

    pub fn git_clone(&self) {
        let mut args = vec![
            "clone",
            &self.repo_url,
            "-b",
            &self.repo_branch,
            "--depth=1",
        ];
        let name = self.name.to_lowercase();
        if let Some(target) = self.project.src.join(name).into_os_string().to_str() {
            args.push(target);
            process::cmd("git", args, ".");
        }
    }

    pub fn build(&mut self) {
        println!("building...{} {}", self.name, self.version);
    }

    pub fn prefix(&self) -> PathBuf {
        self.project.install.join(self.name.to_lowercase())
    }

    pub fn srcdir(&self) -> PathBuf {
        self.project.src.join(self.name.to_lowercase())
    }

    pub fn staticlibs_exist(&self) -> bool {
        for lib in &self.staticlibs {
            if !Path::new(&lib).exists() {
                return false;
            }
        }
        return true;
    }
}

pub fn install_bz2() {
    let bz2 = Dependency::new(
        "bzip2",
        "1.0.8",
        "https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz",
        "https://github.com/libarchive/bzip2.git",
        "bzip2-1.0.8",
        vec![],
        vec!["libbz2.a".to_string()],
    );

    if !bz2.staticlibs_exist() {
        bz2.project.setup();
        bz2.git_clone();
        let prefixopt = format!("PREFIX={}", bz2.prefix().display());
        process::cmd(
            "make",
            vec!["install", &prefixopt, "CFLAGS='-fPIC'"],
            bz2.srcdir(),
        );
        if !bz2.staticlibs_exist() {
            log::error!("could not build bz2");
            std::process::exit(1);
        }
    }
}

pub fn install_ssl() {
    let ssl = Dependency::new(
        "openssl",
        "1.1.1w",
        "https://www.openssl.org/source/old/1.1.1/openssl-1.1.1w.tar.gz",
        "https://github.com/openssl/openssl.git",
        "OpenSSL_1_1_1w",
        vec![],
        vec!["libssl.a".to_string(), "libcrypto.a".to_string()],
    );
    if !ssl.staticlibs_exist() {
        ssl.project.setup();
        ssl.git_clone();
        let prefixopt = format!("--prefix={}", ssl.prefix().display());
        process::cmd(
            "bash",
            vec!["./config", "no-shared", "no-tests", &prefixopt],
            ssl.srcdir(),
        );
        process::cmd("make", vec!["install_sw"], ssl.srcdir());
        if !ssl.staticlibs_exist() {
            log::error!("could not build ssl");
            std::process::exit(1);
        }
    }
}

pub fn install_xz() {}
