extern crate netencode;
extern crate mustache;

use mustache::{Data};
use netencode::{T};
use std::collections::HashMap;
use std::os::unix::ffi::{OsStrExt};
use std::io::{Read};

fn arglib_netencode(env: Option<&std::ffi::OsStr>) -> Result<T, String> {
    let env = match env {
        None => std::ffi::OsStr::from_bytes("ARGLIB_NETENCODE".as_bytes()),
        Some(a) => a
    };
    match std::env::var_os(env) {
        None => Err(format!("could not read args, envvar {} not set", env.to_string_lossy())),
        // TODO: good error handling for the different parser errors
        Some(soup) => match netencode::parse::t_t(soup.as_bytes()) {
            Ok((remainder, t)) => match remainder.is_empty() {
                true => Ok(t),
                false => Err(format!("there was some unparsed bytes remaining: {:?}", remainder))
            },
            Err(err) => Err(format!("parsing error: {:?}", err))
        }
    }
}


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
                .map(|(key, val)| (key, netencode_to_mustache_data_dwim(*val)))
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
        arglib_netencode(None).unwrap()
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
