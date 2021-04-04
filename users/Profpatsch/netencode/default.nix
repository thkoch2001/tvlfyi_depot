{ depot, pkgs, lib, ... }:

let
  imports = {
    inherit (depot.users.Profpatsch)
      writers;
  };

  netencode-rs = imports.writers.rustSimpleLib {
      name = "netencode";
      dependencies = [
        depot.third_party.rust-crates.nom
        depot.users.Profpatsch.execline.exec-helpers
      ];
      release = false;
      verbose = true;
    } (builtins.readFile ./netencode.rs);

  gen = import ./gen.nix { inherit lib; };

  netencode-mustache = imports.writers.rustSimple {
    name = "netencode_mustache";
    dependencies = [
      depot.users.Profpatsch.arglib.netencode.rust
      netencode-rs
      depot.third_party.rust-crates.mustache
    ];
  } (builtins.readFile ./netencode-mustache.rs);


  record-get = imports.writers.rustSimple {
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

  record-splice-env = imports.writers.rustSimple {
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

in depot.nix.utils.drvTargets {
  inherit
   netencode-rs
   netencode-mustache
   record-get
   record-splice-env
   gen
   ;
}
