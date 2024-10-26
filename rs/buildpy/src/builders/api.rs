use std::path::PathBuf;
/// Builder trait for the project
pub trait Builder {
    /// Get the prefix of the builder
    fn prefix(&self) -> PathBuf;

    /// Get the source directory of the builder
    fn src_dir(&self) -> PathBuf;

    /// Get the build directory of the builder
    fn build_dir(&self) -> PathBuf;

    /// Install the dependencies of the builder
    fn install_dependencies(&self);

    /// Download the builder
    fn download(&self);

    /// Setup the builder
    fn setup(&self);

    /// Configure the builder
    fn configure(&self);

    /// Build the builder
    fn build(&self);

    /// Install the builder
    fn install(&self);

    /// Process the builder
    fn process(&self);

    /// Check if the builder is built
    fn is_built(&self) -> bool;
}
