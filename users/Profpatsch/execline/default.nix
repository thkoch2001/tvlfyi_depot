{ depot, pkgs, lib, ... }:

let
  exec-helpers = depot.users.Profpatsch.writers.rustSimpleLib {
    name = "exec-helpers";
  } ''
    use std::os::unix::process::CommandExt;
    use std::ffi::OsStr;
    use std::os::unix::ffi::OsStrExt;
    pub fn exec_into_args<'a, I>(prog_name: &str, env_additions: I) -> !
      where
        I: IntoIterator<Item = (&'a [u8], &'a [u8])>,
    {
        let mut argv = std::env::args_os();
        let prog = argv.nth(1).expect(&format!("{}: first argument must be an executable", prog_name));
        let args = argv;
        let env = env_additions.into_iter().map(|(k,v)| (OsStr::from_bytes(k), OsStr::from_bytes(v)));
        let err = std::process::Command::new(prog).args(args).envs(env).exec();
        panic!("{}: exec failed: {:?}", prog_name, err);
    }
  '';

in {
  inherit
    exec-helpers
    ;
}
