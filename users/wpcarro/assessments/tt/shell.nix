{ pkgs, depot, ... }:

let
  hailgun-src = builtins.fetchGit {
    url = "https://bitbucket.org/echo_rm/hailgun.git";
    rev = "9d5da7c902b2399e0fcf3d494ee04cf2bbfe7c9e";
  };
  hailgun = pkgs.haskellPackages.callCabal2nix "hailgun" hailgun-src {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      hpkgs.servant-server
      hpkgs.aeson
      hpkgs.resource-pool
      hpkgs.sqlite-simple
      hpkgs.wai-cors
      hpkgs.warp
      hpkgs.cryptonite
      hpkgs.uuid
      hpkgs.envy
      hailgun
    ]))
  ];
}
