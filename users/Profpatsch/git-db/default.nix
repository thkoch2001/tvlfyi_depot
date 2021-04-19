{ depot, pkgs, lib, ... }:

depot.nix.writers.rustSimple {
  name = "git-db";
  dependencies = [
    depot.third_party.rust-crates.git2
  ];
} (builtins.readFile ./git-db.rs)
