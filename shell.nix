let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      hpkgs.servant-server
      hpkgs.aeson
      hpkgs.resource-pool
      hpkgs.sqlite-simple
      hpkgs.warp
    ]))
  ];
}
