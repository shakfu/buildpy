/// Project configuration

use std::env;
use std::path::PathBuf;
use crate::ops::log;

/// Project configuration
/// 
/// # Examples
/// 
/// ```
/// use crate::config::project;
/// let project = project::Project::new();
/// ```
#[derive(Debug)]
pub struct Project {
    pub cwd: PathBuf,
    pub build: PathBuf,
    pub downloads: PathBuf,
    pub src: PathBuf,
    pub install: PathBuf,
}

/// Create a new project configuration
impl Project {
    pub fn new() -> Self {
        let _cwd = env::current_dir().unwrap();
        let _build = _cwd.join("build");
        Self {
            cwd: _cwd,
            build: _build.clone(),
            downloads: _build.join("download"),
            src: _build.join("src"),
            install: _build.join("install"),
        }
    }

    /// Setup the project directories
    /// 
    /// # Examples
    /// 
    /// ```
    /// use crate::config::project;
    /// let project = project::Project::new();
    /// project.setup();
    /// ```
    pub fn setup(&self) {
        for p in &[
            self.build.as_path(),
            self.downloads.as_path(),
            self.src.as_path(),
            self.install.as_path(),
        ] {
            match std::fs::create_dir_all(p) {
                Ok(_) => log::info!("created: {}", p.display()),
                Err(e) => log::error!("failure: {e}"),
            };
        }
    }

    /// Clean the project directories
    /// 
    /// # Examples
    /// 
    /// ```
    /// use crate::config::project;
    /// let project = project::Project::new();
    /// project.clean();
    /// ```
    pub fn clean(&self) {
        std::fs::remove_dir_all(self.src.as_path()).unwrap();
    }

    /// Reset the project directories
    /// 
    /// # Examples
    /// 
    /// ```
    /// use crate::config::project;
    /// let project = project::Project::new();
    /// project.reset();
    /// ```
    pub fn reset(&self) {
        std::fs::remove_dir_all(self.build.as_path()).unwrap();
    }
}
