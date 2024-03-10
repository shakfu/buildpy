use std::path::PathBuf;
use std::env;

use crate::ops::shell;

#[derive(Debug)]
pub struct Project {
    pub cwd: PathBuf,
    pub build: PathBuf,
    pub download: PathBuf, 
    pub src: PathBuf, 
    pub install: PathBuf,
}

impl Project {
    pub fn new() -> Self {
        let _cwd = env::current_dir().unwrap();
        let _build = _cwd.join("build");
        Self {
            cwd: _cwd,
            build: _build.clone(),
            download: _build.join("download"),
            src: _build.join("src"),
            install: _build.join("install"),
        }
    }

    pub fn setup(&self) {
        for p in &[self.build.to_str(), self.download.to_str(), self.src.to_str(), self.install.to_str()] {
            if let Some(_p) = p {
                shell::makedirs(_p);
            }
        }
    }
    
    // pub fn clean(&self) {
    //     os.RemoveAll(p.Src)
    // }
    
    // pub fn reset(&self) {
    //     os.RemoveAll(p.Build)
    // }
    


}
