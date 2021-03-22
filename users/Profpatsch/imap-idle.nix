{ depot, pkgs, lib, ... }:

let
  imap-idle = depot.users.Profpatsch.writers.rustSimple {
    name = "imap-idle";
    dependencies = [
      depot.users.Profpatsch.arglib.netencode.rust
      depot.users.Profpatsch.rust-crates.imap
      depot.users.Profpatsch.rust-crates.epoll
      depot.users.Profpatsch.execline.exec-helpers
    ];
  } (builtins.readFile ./imap-idle.rs);

in imap-idle
