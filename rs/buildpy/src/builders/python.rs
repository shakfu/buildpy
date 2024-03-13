use std::path::PathBuf;

use crate::builders;
use crate::builders::api::Builder;
use crate::config;
use crate::ops;
use crate::ops::log;
use crate::ops::process;

use logging_timer::time;

pub struct PythonBuilder {
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
    pub project: config::Project,
}

impl PythonBuilder {
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
            project: config::Project::new(),
        }
    }


    }


impl Builder for PythonBuilder {

    fn install_dependencies(&self) {
        builders::Bzip2Builder::new("1.0.8").process();
        builders::SslBuilder::new("1.1.1w").process();
        builders::XzBuilder::new("5.6.0").process();
    }

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
    
    fn setup(&self) {
        self.project.setup();
        if self.use_git {
            self.git_clone();
        } else {
            let url = self.download_url.replace("<VERSION>", &self.version);
            log::info!("downloading: {}", url);
            ops::download_file(self.project.downloads.clone(), &url);
        }
        self.install_dependencies();
    }

    fn prefix(&self) -> PathBuf {
        self.project.install.join(self.name.to_lowercase())
    }

    fn src_dir(&self) -> PathBuf {
        self.project.src.join(self.name.to_lowercase())
    }

    fn build_dir(&self) -> PathBuf {
        self.project.src.join("build")
    }

    #[time("info")]
    fn build(&mut self) {
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
            self.src_dir(),
        );
        let jobs = format!("-j{}", self.parallel);
        process::cmd("make", vec!["install", &jobs], self.src_dir());
    }

    fn process(&mut self) {
        self.setup();
        self.install_dependencies();
        self.build();
    }

    fn is_built(&self) -> bool {
        self.prefix().join("bin").join("python3").exists()
    }
}
