use crate::core::api;

pub struct Dependency {
    pub name: String,
    pub version: String,
    pub repo: String,
    pub download: String,
}

impl Dependency {
    pub fn new(name: &str, ver: &str, repo: &str, download: &str) -> Self {
        Self {
            name: name.to_string(),
            version: ver.to_string(),
            repo: repo.to_string(),
            download: download.to_string(),
        }
    }
    pub fn build(&mut self) {
        println!("building...{} {}", self.name, self.version);
    }
}

impl api::Buildable for Dependency {
    fn build(&mut self) {
        println!("building...{} {}", self.name, self.version);
    }
}
