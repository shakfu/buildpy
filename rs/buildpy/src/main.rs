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
    cfg.disable_static(vec!["_decimal"]);
    println!("bye..");
}
