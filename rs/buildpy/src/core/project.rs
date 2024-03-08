use std::env;

#[derive(Debug)]
pub struct Project {
    pub cwd: std::path::PathBuf,
}

impl Project {
    pub fn new() -> Self {
        Self {
            cwd: env::current_dir().unwrap(),
        }
    }
}
