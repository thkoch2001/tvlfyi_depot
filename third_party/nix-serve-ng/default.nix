{ pkgs, lib, ... }:

let
  # To update the package to latest git HEAD:
  # $ cabal2nix https://github.com/aristanetworks/nix-serve-ng.git > generated.nix
  # $ depotfmt # cabal2nix doesn't conform to nixpkgs-fmt's expectations
  plainPkg = pkgs.haskellPackages.callPackage ./generated.nix {
    # Needs some header not present in 2.3
    nix = pkgs.nixVersions.nix_2_11;
  };
in

lib.pipe plainPkg (with pkgs.haskell.lib.compose; [
  # cabal file doesn't declare this for some reason
  (addPkgconfigDepend pkgs.boost)
  # only install executable which is statically linked against haskell libs
  justStaticExecutables
])
