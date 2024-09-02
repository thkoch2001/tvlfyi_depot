{ lib, pkgs, ... }:

let
  nix' = pkgs.nixVersions.latest.overrideAttrs (_: {
    version = "2024-08-23";
    src = pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nix";
      rev = "85f1aa6b3df5c5fcc924a74e2a9cc8acea9ba0e1";
      hash = "sha256-3+UgAktTtkGUNpxMxr+q+R+z3r026L3PwJzG6RD2IXM=";
    };
  });
in
{
  PKG_CONFIG_PATH_nix = "${lib.getDev nix'}/lib/pkgconfig";
}
