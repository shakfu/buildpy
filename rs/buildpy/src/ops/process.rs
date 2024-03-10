use crate::ops::log;


pub fn cmd(exec: &str, args: Vec<&str>, cwd: &str) -> () {
    match std::process::Command::new(exec)
        .args(args.clone())
        .current_dir(std::path::Path::new(cwd))
        .status()
    {
        Ok(_) => (),
        Err(err) => {
            log::error!("cmd: '{} {}' failed: {}", exec, args.join(" "), err);
            std::process::exit(1)
        }
    };
}
