let
  depot = (import ./.. {});
  pkgs = depot.third_party.nixpkgs;

in pkgs.mkShell {
  buildInputs = with pkgs; [
    protobuf3_19
    rustup
    rust-analyzer
  ];
}
