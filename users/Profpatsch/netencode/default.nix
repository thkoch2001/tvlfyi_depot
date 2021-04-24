{ depot, pkgs, lib, ... }:

let
  netencode-rs = depot.nix.writers.rustSimpleLib {
      name = "netencode";
      dependencies = [
        depot.third_party.rust-crates.nom
        depot.users.Profpatsch.execline.exec-helpers
      ];
    } (builtins.readFile ./netencode.rs);

  gen = import ./gen.nix { inherit lib; };

  pretty-rs = depot.nix.writers.rustSimpleLib {
    name = "netencode-pretty";
    dependencies = [
      netencode-rs
    ];
  } (builtins.readFile ./pretty.rs);

  pretty = depot.nix.writers.rustSimple {
    name = "netencode-pretty";
    dependencies = [
      netencode-rs
      pretty-rs
      depot.users.Profpatsch.execline.exec-helpers
    ];
  } ''
    extern crate netencode;
    extern crate netencode_pretty;
    extern crate exec_helpers;

    fn main() {
      let (_, prog) = exec_helpers::args_for_exec("netencode-pretty", 0);
      let mut buf = vec![];
      let u = netencode::u_from_stdin_or_die_user_error("netencode-pretty", &mut buf);
      match netencode_pretty::Pretty::from_u(u).print_multiline(&mut std::io::stdout()) {
        Ok(()) => {},
        Err(err) => exec_helpers::die_temporary("netencode-pretty", format!("could not write to stdout: {}", err))
      }
    }
  '';

  netencode-mustache = depot.nix.writers.rustSimple {
    name = "netencode_mustache";
    dependencies = [
      depot.users.Profpatsch.arglib.netencode.rust
      netencode-rs
      depot.third_party.rust-crates.mustache
    ];
  } (builtins.readFile ./netencode-mustache.rs);


  record-get = depot.nix.writers.rustSimple {
    name = "record-get";
    dependencies = [
      netencode-rs
      depot.users.Profpatsch.execline.exec-helpers
      depot.users.Profpatsch.arglib.netencode.rust
    ];
  } ''
    extern crate netencode;
    extern crate arglib_netencode;
    extern crate exec_helpers;
    use netencode::{encode, dec};
    use netencode::dec::{Decoder, DecodeError};

    fn main() {
        let mut buf = vec![];
        let args = exec_helpers::args("record-get", 1);
        let field = match std::str::from_utf8(&args[0]) {
            Ok(f) => f,
            Err(_e) => exec_helpers::die_user_error("record-get", format!("The field name needs to be valid unicode"))
        };
        let u = netencode::u_from_stdin_or_die_user_error("record-get", &mut buf);
        match (dec::RecordDot {field, inner: dec::AnyU }).dec(u) {
            Ok(u) => encode(&mut std::io::stdout(), &u).expect("encoding to stdout failed"),
            Err(DecodeError(err)) => exec_helpers::die_user_error("record-get", err)
        }
    }
  '';

  record-splice-env = depot.nix.writers.rustSimple {
    name = "record-splice-env";
    dependencies = [
      netencode-rs
      depot.users.Profpatsch.execline.exec-helpers
    ];
  } ''
    extern crate netencode;
    extern crate exec_helpers;
    use netencode::dec::{Record, Try, ScalarAsBytes, Decoder, DecodeError};

    fn main() {
        let mut buf = vec![];
        let u = netencode::u_from_stdin_or_die_user_error("record-splice-env", &mut buf);
        let (_, prog) = exec_helpers::args_for_exec("record-splice-env", 0);
        match Record(Try(ScalarAsBytes)).dec(u) {
            Ok(map) => {
                exec_helpers::exec_into_args(
                    "record-splice-env",
                    prog,
                    // some elements can’t be decoded as scalars, so just ignore them
                    map.into_iter().filter_map(|(k, v)| v.map(|v2| (k, v2)))
                );
            },
            Err(DecodeError(err)) => exec_helpers::die_user_error("record-splice-env", err),
        }
    }
  '';

  env-splice-record = depot.nix.writers.rustSimple {
    name = "env-splice-record";
    dependencies = [
      netencode-rs
      depot.users.Profpatsch.execline.exec-helpers
    ];
  } ''
    extern crate netencode;
    extern crate exec_helpers;
    use netencode::{T};
    use std::os::unix::ffi::OsStringExt;

    fn main() {
        exec_helpers::no_args("env-splice-record");
        let mut res = std::collections::HashMap::new();
        for (key, val) in std::env::vars_os() {
          match (String::from_utf8(key.into_vec()), String::from_utf8(val.into_vec())) {
            (Ok(k), Ok(v)) => { let _ = res.insert(k, T::Text(v)); },
            // same as in record-splice-env, we ignore non-utf8 variables
            (_, _) => {},
          }
        }
        netencode::encode(&mut std::io::stdout(), &T::Record(res).to_u()).unwrap()
    }
  '';

in depot.nix.utils.drvTargets {
  inherit
    netencode-rs
    pretty-rs
    pretty
    netencode-mustache
    record-get
    record-splice-env
    env-splice-record
    gen
    ;
}
