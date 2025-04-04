//! Bzip2 builder

use std::path::PathBuf;

// use crate::builders::api::Builder;
use crate::config;
use crate::ops;
use crate::ops::log;
use crate::ops::process;

/// Bzip2 builder
/// 
/// # Examples
/// 
/// ```
/// use crate::builders::bz2;
/// let builder = bz2::Bzip2Builder::new("1.0.8");
/// ```
pub struct Bzip2Builder {
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
    pub project: config::Project,
}

/// Create a new bzip2 builder
impl Bzip2Builder {
    pub fn new(version: &str) -> Self {
        Self {
            name: "bzip2".to_string(),
            version: version.to_string(),
            download_url: format!("https://sourceware.org/pub/bzip2/bzip2-{version}.tar.gz")
                .to_string(),
            repo_url: "https://github.com/libarchive/bzip2.git".to_string(),
            repo_branch: format!("bzip2-{version}").to_string(),
            config_options: vec![],
            staticlibs: vec!["libbz2.a".to_string()],
            use_git: true,
            parallel: 4,
            duration: 0,
            project: config::Project::new(),
        }
    }

    /// Get the prefix of the bzip2 library
    fn prefix(&self) -> PathBuf {
        self.project.install.join(self.name.to_lowercase())
    }

    /// Get the source directory of the bzip2 library
    fn src_dir(&self) -> PathBuf {
        self.project.src.join(self.name.to_lowercase())
    }

    /// Get the build directory of the bzip2 library
    fn build_dir(&self) -> PathBuf {
        self.src_dir().join("build")
    }

    /// Install the dependencies of the bzip2 library
    fn install_dependencies(&self) {}

    /// Git clone the bzip2 repository
    fn git_clone(&self) {
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

    /// Download the bzip2 library
    fn download(&self) {
        if self.use_git {
            self.git_clone();
        } else {
            let url = self.download_url.replace("<VERSION>", &self.version);
            log::info!("downloading: {}", url);
            ops::download_file(self.project.downloads.clone(), &url);
        }
    }

    /// Setup the bzip2 library
    fn setup(&self) {
        self.project.setup();
        self.download();
        self.install_dependencies();
    }

    /// Configure the bzip2 library
    fn configure(&self) {}

    /// Build the bzip2 library
    fn build(&self) {
        println!("building...{} {}", self.name, self.version);
        let prefixopt = format!("PREFIX={}", self.prefix().display());
        process::cmd(
            "make",
            vec!["install", &prefixopt, "CFLAGS='-fPIC'"],
            self.src_dir(),
        );
    }

    /// Install the bzip2 library
    fn install(&self) {}

    /// Process the bzip2 library
    pub fn process(&self) {
        if !self.is_built() {
            self.setup();
            self.build();
            if !self.is_built() {
                log::error!("could not build bzip2");
                std::process::exit(1);
            }
        }
    }

    /// Check if the bzip2 library is built
    fn is_built(&self) -> bool {
        for lib in &self.staticlibs {
            if !self.prefix().join("lib").join(lib).exists() {
                return false;
            }
        }
        true
    }
}
