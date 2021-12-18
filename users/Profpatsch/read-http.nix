{ depot, pkgs, ... }:

let

  read-http = depot.nix.writers.rustSimple
    {
      name = "read-http";
      dependencies = [
        depot.third_party.rust-crates.ascii
        depot.third_party.rust-crates.httparse
        depot.users.Profpatsch.netencode.netencode-rs
        depot.users.Profpatsch.arglib.netencode.rust
        depot.users.Profpatsch.execline.exec-helpers
      ];
    }
    (builtins.readFile ./read-http.rs);

in
read-http
