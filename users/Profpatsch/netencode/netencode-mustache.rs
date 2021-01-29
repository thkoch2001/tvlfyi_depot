extern crate netencode;
extern crate mustache;
extern crate arglib_netencode;

use mustache::{Data};
use netencode::{T};
use std::collections::HashMap;
use std::os::unix::ffi::{OsStrExt};
use std::io::{Read};

fn netencode_to_mustache_data_dwim(t: T) -> Data {
    match t {
        // TODO: good idea?
        T::Unit => Data::Null,
        T::N1(b) => Data::Bool(b),
        T::N3(u) => Data::String(u.to_string()),
        T::N6(u) => Data::String(u.to_string()),
        T::N7(u) => Data::String(u.to_string()),
        T::I3(i) => Data::String(i.to_string()),
        T::I6(i) => Data::String(i.to_string()),
        T::I7(i) => Data::String(i.to_string()),
        T::Text(s) => Data::String(s),
        T::Binary(b) => unimplemented!(),
        T::Sum(tag) => unimplemented!(),
        T::Record(xs) => Data::Map(
            xs.into_iter()
                .map(|(key, val)| (key, netencode_to_mustache_data_dwim(val)))
                .collect::<HashMap<_,_>>()
        ),
        T::List(xs) => Data::Vec(
            xs.into_iter()
                .map(|x| netencode_to_mustache_data_dwim(x))
                .collect::<Vec<_>>()
        ),
    }
}

pub fn from_stdin() -> () {
    let data = netencode_to_mustache_data_dwim(
        arglib_netencode::arglib_netencode(Some(std::ffi::OsStr::new("TEMPLATE_DATA"))).unwrap()
    );
    let mut stdin = String::new();
    std::io::stdin().read_to_string(&mut stdin).unwrap();
    mustache::compile_str(&stdin)
        .and_then(|templ| templ.render_data(
            &mut std::io::stdout(),
            &data
        )).unwrap()
}

pub fn main() {
    from_stdin()
}
