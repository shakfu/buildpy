/// Create a vector of strings
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
pub(crate) use vecs;

/// Create a hashmap of strings to vectors of strings
macro_rules! hashmaps {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(hashmaps!(@single $rest)),*]));

    ($($key:expr => $value:expr,)+) => { hashmaps!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let _cap = hashmaps!(@count $($key),*);
            let mut _map = ::std::collections::HashMap::<String,Vec<String>>::with_capacity(_cap);
            $(
                let _ = _map.insert($key.to_string(),$value);
            )*
            _map
        }
    };
}
pub(crate) use hashmaps;
