//! Python builder


use std::path::PathBuf;

use crate::builders;
// use crate::builders::api::Builder;
use crate::config;
// use crate::config::macros;
use crate::ops;
use crate::ops::log;
use crate::ops::process;
use crate::ops::shell;

use logging_timer::time;

/// Python builder
/// 
/// # Examples
/// 
/// ```
/// use crate::builders::python;
/// let builder = python::PythonBuilder::new("3.12.0");
/// ```
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

/// Create a new python builder
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
            config_options: vec!["--disable-test-modules".to_string()],
            packages: vec![],
            staticlibs: vec![],
            remove_patterns: config::macros::vecs![
                "*.exe",
                "*config-3*",
                "*tcl*",
                "*tdbc*",
                "*tk*",
                "__phello__",
                "__pycache__",
                "_codecs_*.so",
                "_ctypes_test*",
                "_test*",
                "_tk*",
                "_xx*.so",
                "distutils",
                "idlelib",
                "lib2to3",
                "LICENSE.txt",
                "pkgconfig",
                "pydoc_data",
                "site-packages",
                "test",
                "Tk*",
                "turtle*",
                "venv",
                "xx*.so"
            ],
            optimize: false,
            use_git: true,
            parallel: 4,
            duration: 0,
            project: config::Project::new(),
        }
    }

    /// Get the version of the python builder
    fn ver(&self) -> String {
        let xs: Vec<String> = self.version.split('.').map(|s| s.to_string()).collect();
        format!("{}.{}", xs[0], xs[1])
    }

    fn ver_nodot(&self) -> String {
        let xs: Vec<String> = self.version.split('.').map(|s| s.to_string()).collect();
        format!("{}{}", xs[0], xs[1])
    }

    /// Get the name and version of the python builder
    fn name_ver(&self) -> String {
        format!("{}{}", self.name.to_lowercase(), self.ver())
    }

    /// Get the setup local path
    fn setup_local(&self) -> PathBuf {
        self.src_dir().join("Modules").join("Setup.local")
    }

    /// Get the build type of the python builder
    fn build_type(&self) -> String {
        let parts: Vec<String> = self.version.split('_').map(|s| s.to_string()).collect();
        parts[0].clone()
    }

    /// Get the size type of the python builder
    fn size_type(&self) -> String {
        let parts: Vec<String> = self.version.split('_').map(|s| s.to_string()).collect();
        parts[1].clone()
    }

    /// Get the prefix of the python builder
    fn prefix(&self) -> PathBuf {
        self.project.install.join(self.name.to_lowercase())
    }

    /// Get the source directory of the python builder
    fn src_dir(&self) -> PathBuf {
        self.project.src.join(self.name.to_lowercase())
    }

    /// Get the build directory of the python builder
    fn build_dir(&self) -> PathBuf {
        self.project.src.join("build")
    }

    /// Git clone the python repository
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
        };
    }

    /// Install the dependencies of the python builder
    fn install_dependencies(&self) {
        builders::Bzip2Builder::new("1.0.8").process();
        builders::OpensslBuilder::new("1.1.1w").process();
        builders::XzBuilder::new("5.4.6").process();
    }

    /// Download the python source code
    fn download(&self) {
        if self.use_git {
            self.git_clone();
        } else {
            let url = self.download_url.replace("<VERSION>", &self.version);
            log::info!("downloading: {}", url);
            ops::download_file(self.project.downloads.clone(), &url);
        }
    }

    /// Setup the python builder
    fn setup(&self) {
        self.project.setup();
        self.download();
        self.install_dependencies();
    }

    /// Configure the python builder
    fn configure(&mut self) {
        log::info!("configuring {}-{} ...", self.name, self.version);

        let prefixopt = format!("--prefix={}", self.prefix().display());

        let mut opts: Vec<&str> = vec!["./configure", &prefixopt];

        if self.build_type() == "shared" {
            self.config_options.extend(vec![
                "--enable-shared".to_string(),
                "--without-static-libpython".to_string(),
            ]);
        } else if self.build_type() == "framework" {
            self.config_options
                .push(format!("--enable-framework={}", self.prefix().display()))
        }
        if self.optimize {
            self.config_options
                .push("--enable-optimizations".to_string());
        }
        if self.packages.is_empty() {
            self.config_options.push("--without-ensurepip".to_string());
            self.remove_patterns.push("ensurepip".to_string());
        }

        for opt in &self.config_options {
            opts.push(opt.as_str());
        }

        // let mut cfg = config::Config::new(self.config.clone(), self.ver());
        let mut cfg = config::Config::new(self.config.clone(), self.version.clone());
        cfg.configure();
        cfg.write(self.setup_local());

        process::cmd("bash", opts, self.src_dir());
    }

    /// Build the python builder
    #[time("info")]
    fn build(&self) {
        log::info!("building...{}-{}", self.name, self.version);
        let jobs = format!("-j{}", self.parallel);
        process::cmd("make", vec![&jobs], self.src_dir());
    }

    /// Install the python builder
    fn install(&self) {
        log::info!("installing...{}-{}", self.name, self.version);
        process::cmd("make", vec!["install"], self.src_dir());
    }

    /// Clean the python builder
    fn clean(&self) {
        let target = self.prefix().join("lib").join(self.name_ver());
        log::info!("cleaning {}", target.display());
        shell::recursive_remove(
            &self.prefix().join("lib").join(self.name_ver()),
            self.remove_patterns.clone(),
        );
    }

    /// Zip the python builder
    fn ziplib(&self) {
        let tmp_libdynload = self.project.build.join("lib-dynload");
        let tmp_os_py = self.project.build.join("os.py");
        let zip_path = self
            .prefix()
            .join("lib")
            .join(format!("python{}.zip", self.ver_nodot()));
        let lib_path = self.prefix().join("lib").join(self.name_ver());

        let src = self.prefix().join("lib").join(self.name_ver());
        let lib_dynload = src.join("lib-dynload");
        let os_py = src.join("os.py");
        let site_packages = src.join("site-packages");

        // pre-cleanup
        if tmp_libdynload.exists() {
            std::fs::remove_dir_all(tmp_libdynload.clone()).unwrap();
        }
        if tmp_os_py.exists() {
            std::fs::remove_file(tmp_os_py.clone()).unwrap();
        }

        shell::mv(lib_dynload.clone(), tmp_libdynload.clone());
        shell::mv(os_py.clone(), tmp_os_py.clone());

        log::info!("zipping {}", lib_path.display());
        process::cmd("zip", vec!["-r", zip_path.to_str().unwrap(), "."], lib_path);

        std::fs::remove_dir_all(src.clone()).unwrap();

        std::fs::remove_dir_all(self.prefix().join("lib").join("pkgconfig")).unwrap();

        std::fs::create_dir_all(src).unwrap();
        shell::makedirs(site_packages.to_str().unwrap());

        shell::mv(tmp_libdynload, lib_dynload);
        shell::mv(tmp_os_py, os_py);
    }

    /// Process the python builder
    pub fn process(&mut self) {
        self.setup();
        self.install_dependencies();
        self.configure();
        self.build();
        self.install();
        self.clean();
        self.ziplib()
    }

    /// Check if the python builder is built
    fn is_built(&self) -> bool {
        self.prefix().join("bin").join("python3").exists()
    }
}
