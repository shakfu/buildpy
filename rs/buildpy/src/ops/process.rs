// use crate::ops::log as log;

use std::fmt;

struct ShowArgs(Vec<String>);

impl fmt::Display for ShowArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "args:")?;
        for v in &self.0 {
            write!(f, " {}", v)?;
        }
        Ok(())
    }
}

pub fn run(exec: &str, args: &[&str]) {
    let _ = command_run::Command::with_args(exec, args)
        .run();
}

pub fn cmd<P>(exec: &str, args: Vec<&str>, cwd: P)
where
    P: AsRef<std::path::Path>,
{
    let parts: Vec<String> = (args).iter().map(|v| v.to_string()).collect();
    let msg = formatx::formatx!("exe: {exec} args: {:?} command failed ", exec, parts.join(" "));
    std::process::Command::new(exec)
        .args(args)
        .current_dir(cwd)
        .status()
        .unwrap_or_else(|_| { panic!("{}", msg.unwrap()) });
        // .spawn()
}
