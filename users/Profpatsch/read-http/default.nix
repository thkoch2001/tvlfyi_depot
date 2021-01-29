{ depot, pkgs, ... }:

let

  # reads a http request (stdin), and writes all headers to stdout, as netencoded dict
  read-http = depot.users.Profpatsch.writers.rustSimple {
    name = "read-http";
    dependencies = [
      depot.users.Profpatsch.rust-crates.ascii
      depot.users.Profpatsch.rust-crates.httparse
      depot.users.Profpatsch.netencode.netencode-rs
      depot.users.Profpatsch.arglib.netencode.rust
    ];
  } (builtins.readFile ./read-http.rs);

in {
  inherit
    read-http
    ;
}
