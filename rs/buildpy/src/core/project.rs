use std::env;
use std::path::{Path,PathBuf};

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
        for p in &[
            self.build.as_path(),
            self.download.as_path(),
            self.src.as_path(),
            self.install.as_path(),
        ] {
            match std::fs::create_dir_all(p) {
                Ok(_) => log::info!("created: {}", p.display()),
                Err(e) => log::error!("failure: {e}"),
            };
        }
    }

    // pub fn clean(&self) {
    //     os.RemoveAll(p.Src)
    // }

    // pub fn reset(&self) {
    //     os.RemoveAll(p.Build)
    // }
}
