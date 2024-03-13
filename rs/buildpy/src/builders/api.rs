use std::path::PathBuf;

pub trait Builder {
    fn prefix(&self) -> PathBuf;
    fn src_dir(&self) -> PathBuf;
    fn build_dir(&self) -> PathBuf;
    fn install_dependencies(&self);
    fn download(&self);
    fn setup(&self);
    fn configure(&self);
    fn build(&self);
    fn install(&self);
    fn process(&self);
    fn is_built(&self) -> bool;
}
