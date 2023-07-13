{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = depot.nix.sparseTree {
    root = ./.;
    paths = [
      ./Cargo.lock
      ./Cargo.toml
      ./src
    ];
  };

  buildInputs = with pkgs; [
    pkg-config
    openssl
  ];
}
