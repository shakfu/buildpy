// use crate::core::api;
// use crate::core::deps;
use super::Project;
use crate::ops;
use crate::ops::log;
use crate::ops::process;
use crate::core::deps;

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
            config:  String::from(cfg),
            version: String::from(version),
            download_url: String::from(
                format!("https://github.com/python/cpython/archive/refs/tags/v{version}.tar.gz")
            ),
            repo_url: String::from("https://github.com/python/cpython.git"),
            repo_branch: String::from(format!("v{version}")),
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
        let mut args = vec!["clone", &self.repo_url, "-b", &self.repo_branch, "--depth=1"];
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
            let target = self.download_url.replace("<VERSION>", &self.version);
            log::info!("downloading: {}", target);
            ops::download_file(&target);
        }
    }

    pub fn prefix(&self) -> String {
        return self.name.to_lowercase();
    }

    pub fn install_dependencies(&self) {
        deps::install_bzr();
        deps::install_ssl();
        deps::install_xz();
    }

    pub fn build(&mut self) {
        println!("building...{}", self.version);
    }

    pub fn process(&mut self) {
        self.setup();
        self.install_dependencies();
        self.build();
    }
}
