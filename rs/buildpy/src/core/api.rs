use std::path::PathBuf;

pub trait Builder {
   fn setup(&self);
   fn prefix(&self) -> PathBuf;    
   fn src_dir(&self) -> PathBuf;
   fn build_dir(&self) -> PathBuf; 
   fn build(&mut self);
   fn process(&mut self);
   fn is_built(&self) -> bool;
}
