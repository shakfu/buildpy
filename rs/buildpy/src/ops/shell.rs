use crate::ops::log;
// use crate::ops::process;

pub fn makedirs(path: &str) {
    log::info!("makedirs {path}");
    match std::fs::create_dir_all(std::path::Path::new(path)) {
        Ok(_) => log::info!("success: directory created"),
        Err(err) => {
            log::error!("failure: {}", err);
            std::process::exit(1)
        }
    }
}
