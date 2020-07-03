{ pkgs ? (import ../../../. {}).third_party }:

pkgs.haskell.lib.doBenchmark (import (pkgs.haskellPackages.haskellSrc2nix {
  name = "xanthous";
  src = pkgs.gitignoreSource ./.;
  extraCabal2nixOptions = "--hpack";
}))
