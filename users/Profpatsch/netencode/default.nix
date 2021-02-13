{ depot, pkgs, lib, ... }:

let
  imports = {
    inherit (depot.users.Profpatsch)
      writers;
  };

  netencode-rs = imports.writers.testRustSimple
    (imports.writers.rustSimpleLib {
      name = "netencode";
      dependencies = [
        depot.users.Profpatsch.rust-crates.nom
        depot.users.Profpatsch.execline.exec-helpers
      ];
      release = false;
      verbose = true;
    } (builtins.readFile ./netencode.rs));

  gen = import ./gen.nix { inherit lib; };

  cfg-if = pkgs.buildRustCrate {
    pname = "cfg-if";
    version = "1.0.0";
    crateName = "cfg-if";
    sha256 = "1fzidq152hnxhg4lj6r2gv4jpnn8yivp27z6q6xy7w6v0dp6bai9";
  };

  log = pkgs.buildRustCrate {
    pname = "log";
    version = "0.4.11";
    crateName = "log";
    sha256 = "0m6xhqxsps5mgd7r91g5mqkndbh8zbjd58p7w75r330zl4n40l07";
    dependencies = [ cfg-if ];
  };

  serde_derive = pkgs.buildRustCrate {
    pname = "serde";
    crateName = "serde";
    version = "1.0.123";
    sha256 = "05xl2s1vpf3p7fi2yc9qlzw88d5ap0z3qmhmd7axa6pp9pn1s5xc";
  };

  serde = pkgs.buildRustCrate {
    pname = "serde";
    crateName = "serde";
    version = "1.0.123";
    sha256 = "05xl2s1vpf3p7fi2yc9qlzw88d5ap0z3qmhmd7axa6pp9pn1s5xc";
    features = [ "std" ];
  };

  mustache = pkgs.buildRustCrate {
    pname = "mustache";
    version = "0.9.0";
    crateName = "mustache";
    sha256 = "1zgl8l15i19lzp90icgwyi6zqdd31b9vm8w129f41d1zd0hs7ayq";
    dependencies = [ log serde ];
  };

  netencode-mustache = imports.writers.rustSimple {
    name = "netencode_mustache";
    dependencies = [
      depot.users.Profpatsch.arglib.netencode.rust
      netencode-rs
      mustache
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

  env-splice-record = imports.writers.rustSimple {
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
    netencode-mustache
    record-get
    record-splice-env
    env-splice-record
    gen
    ;
}
