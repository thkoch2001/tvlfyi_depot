{ pkgs, ... }:

rec {
  hls-nix = { ghc }: import ./hls.nix { inherit pkgs ghc; };
  ghc884 = hls-nix { ghc = "ghc884"; };
}
