{ depot, pkgs, ... }:

let

  read-http = depot.users.Profpatsch.writers.rustSimple {
    name = "read-http";
    dependencies = [
      depot.users.Profpatsch.rust-crates.ascii
      depot.users.Profpatsch.rust-crates.httparse
      depot.users.Profpatsch.netencode.netencode-rs
      depot.users.Profpatsch.arglib.netencode.rust
      depot.users.Profpatsch.execline.exec-helpers
    ];
  } (builtins.readFile ./read-http.rs);

in read-http
