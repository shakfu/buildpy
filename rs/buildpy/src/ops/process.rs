/// Process operations
/// 
/// # Examples
/// 
/// ```
/// use crate::ops::process;
/// process::cmd("echo", vec!["Hello, world!"], Path::new("/"));
/// ```

use crate::ops::log;
use std::collections::HashMap;
use std::path::Path;

/// Run a command
/// 
/// # Arguments
/// 
/// * `exec` - The command to run
/// * `args` - The arguments to pass to the command
/// * `cwd` - The current working directory
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

/// Run a command with environment variables
/// 
/// # Arguments
/// 
/// * `exec` - The command to run
/// * `args` - The arguments to pass to the command
/// * `cwd` - The current working directory
/// * `envs` - The environment variables to pass to the command
pub fn cmd_env<P>(exec: &str, args: Vec<&str>, cwd: P, envs: HashMap<String, String>)
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
