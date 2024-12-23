
use std::path::PathBuf;

use crate::config::Project;
use crate::ops;
use crate::ops::log;
use crate::ops::process;
use crate::ops::shell;

// use crate::builders::api::Builder;

/// Xz builder
/// 
/// # Examples
/// 
/// ```
/// use crate::builders::xz;
/// let builder = xz::XzBuilder::new("5.4.5");
/// ```
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
    pub project: Project,
}

/// Create a new xz builder
impl XzBuilder {
    pub fn new(version: &str) -> Self {
        Self {
            name: "xz".to_string(),
            version: version.to_string(),
            download_url:
                format!("https://github.com/tukaani-project/xz/releases/download/v{version}/xz-{version}.tar.gz")
                    .to_string(),
            repo_url: "https://github.com/python/cpython-source-deps.git".to_string(),
            repo_branch: "xz".to_string(),
            config_options: vec![],
            staticlibs: vec!["liblzma.a".to_string()],
            use_git: true,
            parallel: 4,
            duration: 0,
            project: Project::new(),
        }
    }

    /// Get the prefix of the xz library
    fn prefix(&self) -> PathBuf {
        self.project.install.join(self.name.to_lowercase())
    }

    /// Get the source directory of the xz library
    fn src_dir(&self) -> PathBuf {
        self.project.src.join(self.name.to_lowercase())
    }

    /// Get the build directory of the xz library
    fn build_dir(&self) -> PathBuf {
        self.src_dir().join("build")
    }

    /// Install the dependencies of the xz library
    fn install_dependencies(&self) {}

    /// Git clone the xz repository
    fn git_clone(&self) {
        let mut args = vec![
            "clone",
            &self.repo_url,
            "-b",
            &self.repo_branch,
            "--depth=1",
        ];
        if let Some(target) = self.src_dir().into_os_string().to_str() {
            args.push(target);
            process::cmd("git", args, ".");
        }
    }

    /// Download the xz library
    fn download(&self) {
        if self.use_git {
            self.git_clone();
        } else {
            let url = self.download_url.replace("<VERSION>", &self.version);
            log::info!("downloading: {}", url);
            ops::download_file(self.project.downloads.clone(), &url);
        }
    }

    /// Setup the xz library
    fn setup(&self) {
        self.project.setup();
        self.download();
        self.install_dependencies();
        let src_dir = self.src_dir();
        let cwd = src_dir.to_str().unwrap();
        shell::chmod("configure", 755, cwd);
        shell::chmod("build-aux/install-sh", 755, cwd);
    }

    /// Configure the xz library
    fn configure(&self) {
        println!("configuring...{} {}", self.name, self.version);
        let prefixopt = format!("--prefix={}", self.prefix().display());
        let opts = vec![
            "configure",
            "--disable-dependency-tracking",
            "--disable-xzdec",
            "--disable-lzmadec",
            "--disable-nls",
            "--enable-small",
            "--disable-shared",
             &prefixopt
        ];
        process::cmd("/bin/sh", opts, self.src_dir());
    }

    /// Build the xz library
    fn build(&self) {
        println!("building...{} {}", self.name, self.version);
        process::cmd("make", vec!["install"], self.src_dir());
    }

    /// Install the xz builder
    fn install(&self) {}

    // fn configure(&self) {
    //     println!("configuring...{} {}", self.name, self.version);
    //     let envs = HashMap::from([("CFLAGS".to_string(), "-fPIC".to_string())]);
    //     let opts = vec![
    //         "-DBUILD_SHARED_LIBS=OFF",
    //         "-DENABLE_NLS=OFF",
    //         "-DENABLE_SMALL=ON",
    //         "-DCMAKE_BUILD_TYPE=MinSizeRel",
    //     ];
    //     if let Some(sdir) = self.src_dir().to_str() {
    //         if let Some(bdir) = self.build_dir().to_str() {
    //             shell::cmake_configure_env(sdir, bdir, opts, envs.clone());
    //         }
    //     }
    // }

    // fn build(&self) {
    //     println!("building...{} {}", self.name, self.version);
    //     let envs = HashMap::from([("CFLAGS".to_string(), "-fPIC".to_string())]);
    //     if let Some(bdir) = self.build_dir().to_str() {
    //         shell::cmake_build_env(bdir, true, envs);
    //     }
    // }

    // fn install(&self) {
    //     println!("installing...{} {}", self.name, self.version);
    //     if let Some(bdir) = self.build_dir().to_str() {
    //         if let Some(pdir) = self.prefix().to_str() {
    //             shell::cmake_install(bdir, pdir);
    //         }
    //     }
    // }

    /// Process the xz builder
    pub fn process(&self) {
        if !self.is_built() {
            self.setup();
            self.configure();
            self.build();
            self.install();
            if !self.is_built() {
                log::error!("could not build xz");
                std::process::exit(1);
            }
        }
    }

    /// Check if the xz builder is built
    fn is_built(&self) -> bool {
        for lib in &self.staticlibs {
            if !self.prefix().join("lib").join(lib).exists() {
                return false;
            }
        }
        true
    }
}
