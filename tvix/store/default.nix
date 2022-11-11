{ depot, pkgs, lib, ... }:

lib.fix (self: depot.third_party.naersk.buildPackage (lib.fix (naerskArgs: {
  src = depot.third_party.gitignoreSource ./.;
  # see https://github.com/nix-community/naersk/issues/169
  root = depot.nix.sparseTree ./. [ ./Cargo.lock ./Cargo.toml ];

  doCheck = true;
}))
)
