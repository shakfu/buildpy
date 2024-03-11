use std::path::Path;
use std::collections::HashMap;
use crate::ops::log;

pub fn cmd<P>(exec: &str, args: Vec<&str>, cwd: P)
where
    P: AsRef<Path>,
{
    match std::process::Command::new(exec)
        .args(args.clone())
        .current_dir(&cwd)
        .status()
    {
        Ok(_) => (),
        Err(err) => {
            log::error!("cmd: '{} {}' failed: {}", exec, args.join(" "), err);
            std::process::exit(1)
        }
    };
}


pub fn cmd_env<P>(exec: &str, args: Vec<&str>, cwd: P, envs: HashMap<String,String>)
where
    P: AsRef<Path>,
{
    match std::process::Command::new(exec)
        .args(args.clone())
        .current_dir(&cwd)
        .envs(envs)
        .status()
    {
        Ok(_) => (),
        Err(err) => {
            log::error!("cmd: '{} {}' failed: {}", exec, args.join(" "), err);
            std::process::exit(1)
        }
    };
}
