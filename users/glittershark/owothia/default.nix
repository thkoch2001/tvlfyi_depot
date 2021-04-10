{ pkgs ? (import ../../../. {}).third_party, ... }:

let
  basePkg = pkgs.haskellPackages.callPackage ./pkg.nix { };
in

pkgs.haskell.lib.overrideSrc basePkg {
  src = pkgs.gitignoreSource ./.;
  version = "canon";
}
