#[allow(dead_code)]

mod config;


fn main() {
    let mut cfg = config::Config::new(
        "static_max".to_string(), 
        "3.12.2".to_string()
    );
    println!("cfg is {:?}", cfg);
    for key in cfg.exts.keys() {
        println!("{key}");
    }
    cfg.static_to_disabled(vec!["_decimal"]);
    println!("bye..");
}
