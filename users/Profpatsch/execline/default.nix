{ depot, pkgs, lib, ... }:

let
  exec-helpers-hs = pkgs.haskellPackages.mkDerivation {
    pname = "exec-helpers";
    version = "0.1.0";

    src = depot.users.Profpatsch.exactSource ./. [
      ./exec-helpers.cabal
      ./ExecHelpers.hs
    ];

    libraryHaskellDepends = [
      depot.users.Profpatsch.my-prelude
    ];

    isLibrary = true;
    license = lib.licenses.mit;
  };

  print-one-env = depot.nix.writers.rustSimple
    {
      name = "print-one-env";
      dependencies = [
        depot.users.Profpatsch.execline.exec-helpers
      ];
    } ''
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

  setsid = depot.nix.writers.rustSimple
    {
      name = "setsid";
      dependencies = [
        depot.users.Profpatsch.execline.exec-helpers
        depot.third_party.rust-crates.libc
      ];
    } ''
    use std::os::unix::ffi::OsStrExt;
    use std::ffi::OsStr;

    fn main() {
      let (args, prog) = exec_helpers::args_for_exec("setsid", 1);
      let envvar = OsStr::from_bytes(&args.get(0).expect("first argument must be envvar name to set"));
      let sid: i32 = unsafe { libc::setsid() };
      std::env::set_var(envvar, format!("{}", sid));
      let env: Vec<(&[u8], &[u8])> = vec![];
      exec_helpers::exec_into_args("getid", prog, env);
    }
  '';

in
depot.nix.readTree.drvTargets {
  inherit
    exec-helpers-hs
    print-one-env
    setsid
    ;
}
