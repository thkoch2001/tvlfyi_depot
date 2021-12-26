let
  depot = import ../../.. {};
in
depot.third_party.nixpkgs.mkShell {
  buildInputs = [
    depot.third_party.agenix.cli
  ];
}
