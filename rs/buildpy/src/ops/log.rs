/// Logging operations
/// 
/// # Examples
/// 
/// ```
/// use crate::ops::log;
/// log::init_logging();
/// ```

pub use simplelog::{debug, error, info, warn};

/// Initialize logging
pub fn init_logging() {
    simplelog::CombinedLogger::init(vec![
        simplelog::TermLogger::new(
            simplelog::LevelFilter::Debug,
            simplelog::Config::default(),
            simplelog::TerminalMode::Mixed,
            simplelog::ColorChoice::Auto,
        ),
        simplelog::WriteLogger::new(
            simplelog::LevelFilter::Info,
            simplelog::Config::default(),
            std::fs::File::create("buildpy.log").unwrap(),
        ),
    ])
    .unwrap();

    log_panics::init();
}
