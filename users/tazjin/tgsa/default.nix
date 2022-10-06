{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = depot.nix.sparseTree ./. [
    ./Cargo.lock
    ./Cargo.toml
    ./src
  ];

  buildInputs = with pkgs; [
    pkg-config
    openssl
  ];
}
