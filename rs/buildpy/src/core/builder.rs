// use crate::core::api;
// use crate::core::deps;
use crate::ops;
use crate::ops::log as log;
use super::Dependency;

pub struct Builder {
	pub name: String,
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
}

impl Builder {
    pub fn new() -> Self {
        Self {
            name: String::from("Python"),
            version: String::from("3.11.8"),
            download_url: String::from("https://github.com/python/cpython/archive/refs/tags/v<VERSION>.tar.gz"),
            repo_url: String::from("https://github.com/python/cpython.git"),
            repo_branch: String::from("v<VERSION>"),
            config_options: vec![],
            packages: vec![],
            staticlibs: vec![],
            remove_patterns: vec![],
            optimize: false, 
            use_git: true,
            parallel: 4,
            duration: 0,
    	}
    }
    pub fn setup(&self) {
        if self.use_git {
            let branch = self.repo_branch.replace("<VERSION>", &self.version);
            ops::git_clone(&self.repo_url, &branch, ".");
        } else {
            let target = self.download_url.replace("<VERSION>", &self.version);
            log::info!("downloading: {}", target);
            ops::download_file(&target);    
        }
    }

    pub fn install_dependencies(&self) {
        let mut _deps: Dependency = Dependency::new(
            "bz2", 
            "0.0.1",
            "https://github.com/shakfu/bzr.git",
            "https://github.com/shakfu/bzr/releases/bzr.tgz",
        );
        _deps.build();
    }

    pub fn build(&mut self) {
        println!("building...{}", self.version);
    }

    pub fn process(&mut self) {
        // self.setup();
        self.install_dependencies();
        self.build();
    }


}

