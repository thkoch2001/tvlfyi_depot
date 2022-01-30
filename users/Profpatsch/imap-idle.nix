{ depot, pkgs, lib, ... }:

let
  imap-idle = depot.nix.writers.rustSimple
    {
      name = "imap-idle";
      dependencies = [
        depot.users.Profpatsch.arglib.netencode.rust
        depot.third_party.rust-crates.imap
        depot.third_party.rust-crates.epoll
        depot.users.Profpatsch.execline.exec-helpers
      ];
    }
    (builtins.readFile ./imap-idle.rs);

in
imap-idle
