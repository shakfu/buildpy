use std::collections::HashMap;
use std::path::PathBuf;

use crate::config;
use crate::ops::log;
use crate::ops::process;
use crate::ops::shell;

use crate::builders::api::Builder;

pub struct XzBuilder {
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

impl XzBuilder {
    pub fn new() -> Self {
        Self {
            name: "xz".to_string(),
            version: "5.6.0".to_string(),
            download_url:
                "https://github.com/tukaani-project/xz/releases/download/v5.6.0/xz-5.6.0.tar.gz"
                    .to_string(),
            repo_url: "https://github.com/tukaani-project/xz.git".to_string(),
            repo_branch: "v5.6.0".to_string(),
            config_options: vec![],
            staticlibs: vec!["liblzma.a".to_string()],
            use_git: true,
            parallel: 4,
            duration: 0,
            project: config::Project::new(),
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

impl Builder for XzBuilder {
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
            let envs = HashMap::from([("CFLAGS".to_string(), "-fPIC".to_string())]);
            let opts = vec![
                "-DBUILD_SHARED_LIBS=OFF",
                "-DENABLE_NLS=OFF",
                "-DENABLE_SMALL=ON",
                "-DCMAKE_BUILD_TYPE=MinSizeRel",
            ];
            if let Some(sdir) = self.src_dir().to_str() {
                if let Some(bdir) = self.build_dir().to_str() {
                    if let Some(pdir) = self.prefix().to_str() {
                        shell::cmake_configure_env(sdir, bdir, opts, envs.clone());
                        shell::cmake_build_env(bdir, true, envs);
                        shell::cmake_install(bdir, pdir);
                        if !self.is_built() {
                            log::error!("could not build xz");
                            std::process::exit(1);
                        }
                    }
                }
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
