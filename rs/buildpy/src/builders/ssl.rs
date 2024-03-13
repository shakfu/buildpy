use std::path::PathBuf;

use crate::builders::api::Builder;
use crate::config::Project;
use crate::ops::log;
use crate::ops::process;

pub struct SslBuilder {
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
    pub project: Project,
}

impl SslBuilder {
    pub fn new(version: &str) -> Self {
        Self {
            name: "openssl".to_string(),
            version: version.to_string(),
            download_url: format!(
                "https://www.openssl.org/source/old/1.1.1/openssl-{version}.tar.gz"
            )
            .to_string(),
            repo_url: "https://github.com/openssl/openssl.git".to_string(),
            repo_branch: format!("OpenSSL_{}", version.replace('_', ".")).to_string(),
            config_options: vec!["no-shared".to_string(), "no-tests".to_string()],
            staticlibs: vec!["libssl.a".to_string(), "libcrypto.a".to_string()],
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
}

impl Builder for SslBuilder {
    fn setup(&self) {
        self.project.setup();
        self.git_clone();
    }

    fn build(&mut self) {
        println!("building...{} {}", self.name, self.version);
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

    fn process(&mut self) {
        if !self.is_built() {
            self.setup();
            let prefixopt = format!("--prefix={}", self.prefix().display());
            process::cmd(
                "bash",
                vec!["./config", "no-shared", "no-tests", &prefixopt],
                self.src_dir(),
            );
            process::cmd("make", vec!["install_sw"], self.src_dir());
            if !self.is_built() {
                log::error!("could not build ssl");
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
