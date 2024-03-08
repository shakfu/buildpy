#[allow(dead_code)]
mod config;
mod shell;

fn main() {
    let mut cfg = config::Config::new("static_max".to_string(), "3.12.2".to_string());

    let serialized = serde_yaml::to_string(&cfg).unwrap();
    println!("{serialized}");

    // println!("cfg is {:?}", cfg);
    for key in cfg.exts.keys() {
        println!("{key}");
    }
    cfg.static_to_disabled(vec!["_decimal"]);
    println!("bye..");

    shell::cmd("pytho", vec!["--version", "hello", "GREAT!"], ".");
}
