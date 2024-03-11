use std::path::PathBuf;

use super::Project;
use crate::core::deps;
use crate::ops;
use crate::ops::log;
use crate::ops::process;

pub struct Builder {
    pub name: String,
    pub config: String,
    pub version: String,
    pub download_url: String,
    pub repo_url: String,
    pub repo_branch: String,
    pub config_options: Vec<String>,
    pub packages: Vec<String>,
    pub staticlibs: Vec<String>,
    pub remove_patterns: Vec<String>,
    pub optimize: bool,
    pub use_git: bool,
    pub parallel: i16, // n workers
    pub duration: i16, // seconds
    pub project: Project,
}

impl Builder {
    pub fn new(cfg: &str, version: &str) -> Self {
        Self {
            name: String::from("Python"),
            config: String::from(cfg),
            version: String::from(version),
            download_url: format!(
                "https://github.com/python/cpython/archive/refs/tags/v{version}.tar.gz"
            ),
            repo_url: String::from("https://github.com/python/cpython.git"),
            repo_branch: format!("v{version}"),
            config_options: vec![],
            packages: vec![],
            staticlibs: vec![],
            remove_patterns: vec![],
            optimize: false,
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

    pub fn setup(&self) {
        self.project.setup();
        if self.use_git {
            self.git_clone();
        } else {
            let url = self.download_url.replace("<VERSION>", &self.version);
            log::info!("downloading: {}", url);
            ops::download_file(self.project.downloads.clone(), &url);
        }
    }

    pub fn prefix(&self) -> PathBuf {
        self.project.install.join(self.name.to_lowercase())
    }

    pub fn srcdir(&self) -> PathBuf {
        self.project.src.join(self.name.to_lowercase())
    }

    pub fn install_dependencies(&self) {
        deps::install_bz2();
        deps::install_ssl();
        deps::install_xz();
    }

    pub fn build(&mut self) {
        log::info!("building...{}-{}", self.name, self.version);
        let prefixopt = format!("--prefix={}", self.prefix().display());
        process::cmd(
            "bash",
            vec![
                "./configure",
                "--disable-test-modules",
                "--enable-shared",
                "--without-static-libpython",
                &prefixopt,
            ],
            self.srcdir(),
        );
        let jobs = format!("-j{}", self.parallel);
        process::cmd("make", vec!["install", &jobs], self.srcdir());
    }

    pub fn process(&mut self) {
        self.setup();
        self.install_dependencies();
        self.build();
    }
}
