#![allow(dead_code)]
#![allow(clippy::vec_init_then_push)]

mod config;
mod core;
mod ops;

use crate::ops::log;
use crate::ops::process;

use clap::Parser;
use std::env;

// use std::path::PathBuf;

/// Builds python from source
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Python version
    #[arg(short, long, default_value = "3.11.8")]
    pyversion: Option<String>,

    /// Config name
    #[arg(short, long, default_value = "static_max")]
    config: Option<String>,

    /// Config options
    #[arg(short, long)]
    opts: Option<Vec<String>>,

    /// Run Demo
    #[arg(short, long, action)]
    demo: bool,
    // version, _ := cmd.Flags().GetString("version")
    // config, _ := cmd.Flags().GetString("config")
    // pkgs, _ := cmd.Flags().GetStringSlice("pkgs")
    // opts, _ := cmd.Flags().GetStringSlice("opts")
    // jobs, _ := cmd.Flags().GetInt("jobs")
    // optimize, _ := cmd.Flags().GetBool("optimize")
    // reset, _ := cmd.Flags().GetBool("reset")
    // debug, _ := cmd.Flags().GetBool("debug")
    // git, _ := cmd.Flags().GetBool("git")
}

/// run a demo
fn run_demo() {
    let mut cfg = config::Config::new("static_max".to_string(), "3.12.2".to_string());

    let _serialized = serde_yaml::to_string(&cfg).unwrap();

    process::cmd("python3", vec!["-c", "print('ok')"], ".");

    println!("{_serialized}");

    println!("cfg is {:?}", cfg);
    for key in cfg.exts.keys() {
        println!("{key}");
    }
    cfg.static_to_disabled(config::macros::vecs!["_decimal"]);
    println!("bye..");

    println!("{}", env::consts::OS); // Prints the current OS.

    log::debug!("This level is currently not enabled for any logger");
    log::info!("This only appears in the log file");
    log::warn!("This is a warning");
    log::error!("Bright red error");

    println!("this should fail end exit:");
    process::cmd("python2", vec!["--version"], ".");
    // ops::cmd("python2", &["--version"], ".");
}

// fn run_builder() {
//     let mut _builder = core::Builder::new();
//     _builder.process();
// }

fn main() {
    log::init_logging();

    let args = Cli::parse();

    // if let Some(name) = args.name.as_deref() {
    //     log::info!("name: {name}");
    // }
    if args.demo {
        log::info!("run demo: {}", args.demo);
        run_demo();
    } else if let Some(version) = args.pyversion.as_deref() {
        if let Some(cfg) = args.config.as_deref() {
            log::info!("pyversion: {version} config: {cfg}");
            let mut builder = core::Builder::new(cfg, version);
            builder.process();
        }
    }

    // if let Some(ver) = args.pyversion.as_deref() {
    //     log::info!("pyversion: {ver}");
    // }

    // if let Some(cfg) = args.config.as_deref() {
    //     log::info!("config: {cfg}");
    // }

    // if let Some(opts) = args.opts.as_deref() {
    //     log::info!("opts: {opts:?}");
    // }
}
