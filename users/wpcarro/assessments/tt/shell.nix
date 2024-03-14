{ pkgs, depot, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (
      hpkgs: with hpkgs; [
        hpkgs.aeson
        hpkgs.cryptonite
        hpkgs.envy
        hpkgs.hailgun
        hpkgs.resource-pool
        hpkgs.servant-server
        hpkgs.sqlite-simple
        hpkgs.uuid
        hpkgs.wai-cors
        hpkgs.warp
      ]
    ))
  ];
}
