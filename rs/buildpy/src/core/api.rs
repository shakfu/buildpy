pub trait Buildable {
    fn build(&mut self);
    fn speak(&mut self, x: String) -> String;
}

struct Person {
    name: String,
}

impl Buildable for Person {
    fn build(&mut self) {
        println!("OK");
    }

    fn speak(&mut self, x: String) -> String {
        return x.clone();
    }
}