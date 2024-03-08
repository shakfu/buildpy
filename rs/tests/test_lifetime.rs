#[derive(Debug)]
pub struct Config<'a> {
    pub name: String,
    pub version: String,
    pub active: Vec<&'a str>,
    pub disabled: Vec<&'a str>,
}

impl<'a> Config<'a> {
    pub fn new(name: String, version: String) -> Self {
        Self {
            name: name.clone(),
            version: version.clone(),
            active: vec!["a", "b", "c"],
            disabled: vec!["d", "e", "f"],
        }
    }

    pub fn disable(&mut self, names: Vec<&'a str>) {
        for name in names {
            self.disabled.push(name);
            self.active.retain(|&x| x != name);
        }
    }

    pub fn enable(&mut self, names: Vec<&'a str>) {
        for name in names {
            self.active.push(name);
            self.disabled.retain(|&x| x != name);
        }
    }
}

fn main() {
    let mut cfg = Config::new("foo".to_string(), "1.1.0".to_string());
    println!("cfg before is {:?}", cfg);
    cfg.disable(vec!["a", "c"]);
    println!("\ncfg after is {:?}", cfg);
    cfg.enable(vec!["a"]);
    println!("\nfinally: {:?}", cfg);
}
