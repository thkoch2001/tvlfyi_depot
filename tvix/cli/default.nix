{ depot, pkgs, lib, ... }:

depot.third_party.naersk.buildPackage {
  src = depot.third_party.gitignoreSource ./.;
  # see https://github.com/nix-community/naersk/issues/169
  root = depot.tvix.naerskRootFor ./Cargo.toml;
  doCheck = true;
}
