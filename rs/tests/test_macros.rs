use std::collections::HashMap;


pub struct Config {
	pub name: String,
	pub version: String,
	pub headers: Vec<String>,
	pub exts: HashMap<String, Vec<String>>,
	pub core: Vec<String>,
	pub shared: Vec<String>,
	pub disabled: Vec<String>,
}


macro_rules! vecs {
    ( $( $x:expr ),* ) => {
        {
            let mut _vec = Vec::<String>::new();
            $(
                _vec.push($x.to_string());
            )*
            _vec
        }
    };
}

macro_rules! hashmap {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(hashmap!(@single $rest)),*]));

    ($($key:expr => $value:expr,)+) => { hashmap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let _cap = hashmap!(@count $($key),*);
            let mut _map = ::std::collections::HashMap::<String,Vec<String>>::with_capacity(_cap);
            $(
                let _ = _map.insert($key.to_string(),$value);
            )*
            _map
        }
    };
}

pub fn get_map() ->  HashMap<String, Vec<String>> {
	let map = hashmap!{
		"a" => vecs!["sa", "ba", "ds"],
		"b" => vecs!["dsbud", "ok"],
	};
	return map;
}



pub fn get_strings() -> Vec<String> {
	let xs = vecs!["DESTLIB=$(LIBDEST)", "MACHDESTLIB=$(BINLIBDEST)"];
	return xs;
}


impl Config {
    pub fn new(name: String, version: String) -> Self {
        Self {
	        name: name.clone(),
	        version: version.clone(),
	        headers: vecs![
		        "DESTLIB=$(LIBDEST)",
		        "MACHDESTLIB=$(BINLIBDEST)"
	        ],
	        exts: hashmap!{
	            "name" => vecs!["name", "dest"],
		        "_abc" => vecs!["_abc.c"],
		        "zlib" => vecs!["zlibmodule.c", "-lz"],
	        },
	        core: vecs![
		        "_abc",
		        "_codecs",
		        "unicodedata",
		        "zlib"
	        ],
	        shared: vec![],
	        disabled: vecs![
		        "_codecs_cn",
		        "_codecs_hk"
	        ],
        }
    }
}