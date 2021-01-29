{ depot, pkgs, lib, ... }:

let
  netencode = {
    rust = depot.users.Profpatsch.writers.rustSimpleLib {
      name = "arglib-netencode";
      dependencies = [
        depot.users.Profpatsch.netencode.netencode-rs
      ];
    } ''
      extern crate netencode;

      use netencode::{T};
      use std::os::unix::ffi::OsStrExt;

      pub fn arglib_netencode(env: Option<&std::ffi::OsStr>) -> Result<T, String> {
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
    '';
  };

in {
   inherit
    netencode
    ;
}
