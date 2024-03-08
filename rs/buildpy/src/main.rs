#[allow(dead_code)]
mod config;
mod core;
mod ops;

use clap::Parser;

// extern crate simplelog;
use std::env;

// use crate::core::Dependency;

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
            std::fs::File::create("my_rust_binary.log").unwrap(),
        ),
    ])
    .unwrap();

    println!("{}", env::consts::OS); // Prints the current OS.

    simplelog::error!("Bright red error");
    simplelog::info!("This only appears in the log file");
    simplelog::debug!("This level is currently not enabled for any logger");
}
