{ depot
, pkgs
, lib
, ...
}:
let
  exec-helpers = depot.nix.writers.rustSimpleLib { name = "exec-helpers"; } ( builtins.readFile ./exec_helpers.rs );
  print-one-env =
    depot.nix.writers.rustSimple
      { name = "print-one-env"; dependencies = [ depot.users.Profpatsch.execline.exec-helpers ]; }
      ''
      extern crate exec_helpers;
      use std::os::unix::ffi::OsStrExt;
      use std::io::Write;

      fn main() {
        let args = exec_helpers::args("print-one-env", 1);
        let valname = std::ffi::OsStr::from_bytes(&args[0]);
        match std::env::var_os(&valname) {
          None => exec_helpers::die_user_error("print-one-env", format!("Env variable `{:?}` is not set", valname)),
          Some(val) => std::io::stdout().write_all(&val.as_bytes()).unwrap()
        }
      }
      '';
in
depot.nix.readTree.drvTargets { inherit exec-helpers print-one-env; }
