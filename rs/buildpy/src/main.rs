#[allow(dead_code)]
mod config;
mod core;
mod ops;
use ops::log as log;

use clap::Parser;
use std::env;

// extern crate simplelog;
//pub use simplelog::{info,debug,warn,error};

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Python version
    #[arg(short, long, default_value = "3.11.8")]
    pyversion: String,

    /// Config name
    #[arg(short, long, default_value = "static_max")]
    name: String,
}

fn main() {
    log::init_logging();

    let _args = Args::parse();

    let cfg = config::Config::new("static_max".to_string(), "3.12.2".to_string());

    let _proj = core::Project::new();
    println!("project cwd: {:?}", _proj.cwd);

    let mut _builder = core::Builder::new();
    _builder.process();

    let _serialized = serde_yaml::to_string(&cfg).unwrap();

    ops::run("python3", &["-c", "print('ok')"]);

    // println!("{serialized}");

    // // println!("cfg is {:?}", cfg);
    // for key in cfg.exts.keys() {
    //     println!("{key}");
    // }
    // // cfg.static_to_disabled(vecs!["_decimal"]);
    // println!("bye..");

    ops::cmd("python3", vec!["--version"], ".");


    println!("{}", env::consts::OS); // Prints the current OS.

    log::debug!("This level is currently not enabled for any logger");
    log::info!("This only appears in the log file");
    log::warn!("This is a warning");
    log::error!("Bright red error");

}
