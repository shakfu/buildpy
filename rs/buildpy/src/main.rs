#[allow(dead_code)]
mod config;
mod shell;

use clap::Parser;



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

    let serialized = serde_yaml::to_string(&cfg).unwrap();
    println!("{serialized}");

    // println!("cfg is {:?}", cfg);
    for key in cfg.exts.keys() {
        println!("{key}");
    }
    // cfg.static_to_disabled(vecs!["_decimal"]);
    println!("bye..");

    shell::cmd("pytho", vec!["--version", "hello", "GREAT!"], ".");
}

