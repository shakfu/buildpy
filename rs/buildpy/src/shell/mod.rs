use std::{path::Path, process::Command};
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




pub fn cmd<P>(exec: &str, args: Vec<&str>, cwd: P) 
where
    P: AsRef<Path>
{
    let parts: Vec<String> = (args).iter().map(|v| v.to_string()).collect();
    Command::new(exec)
        .args(args)
        .current_dir(cwd)
        .spawn()
        .expect(&format!("exe: {exec} {} command failed", ShowArgs(parts)));
}

