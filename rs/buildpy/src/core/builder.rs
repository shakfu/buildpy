// use crate::core::api;
// use crate::core::deps;
use crate::ops;
use crate::ops::log as log;
use super::Dependency;

pub struct Builder {
	pub name: String,
    pub version: String,
    pub repo: String,
    pub archive: String,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            name: String::from("Python"),
            version: String::from("3.11.8"),
            repo: String::from("https://github.com/python/cpython.git"),
            archive: String::from("https://github.com/python/cpython/archive/refs/tags/v<VERSION>.tar.gz"),
    	}
    }
    pub fn setup(&self) {
        let target = self.archive.replace("<VERSION>", &self.version);
        log::info!("downloading: {}", target);
        ops::download::download_file(&target);
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
        self.setup();
        self.install_dependencies();
        self.build();
    }


}

