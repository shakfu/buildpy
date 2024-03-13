use std::path::PathBuf;

// use crate::builders::api::Builder;
use crate::config;
use crate::ops;
use crate::ops::log;
use crate::ops::process;

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

    fn prefix(&self) -> PathBuf {
        self.project.install.join(self.name.to_lowercase())
    }

    fn src_dir(&self) -> PathBuf {
        self.project.src.join(self.name.to_lowercase())
    }

    fn build_dir(&self) -> PathBuf {
        self.src_dir().join("build")
    }

    fn install_dependencies(&self) {}

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

    fn download(&self) {
        if self.use_git {
            self.git_clone();
        } else {
            let url = self.download_url.replace("<VERSION>", &self.version);
            log::info!("downloading: {}", url);
            ops::download_file(self.project.downloads.clone(), &url);
        }
    }

    fn setup(&self) {
        self.project.setup();
        self.download();
        self.install_dependencies();
    }

    fn configure(&self) {}

    fn build(&self) {
        println!("building...{} {}", self.name, self.version);
        let prefixopt = format!("PREFIX={}", self.prefix().display());
        process::cmd(
            "make",
            vec!["install", &prefixopt, "CFLAGS='-fPIC'"],
            self.src_dir(),
        );
    }

    fn install(&self) {}

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

    fn is_built(&self) -> bool {
        for lib in &self.staticlibs {
            if !self.prefix().join("lib").join(lib).exists() {
                return false;
            }
        }
        true
    }
}
