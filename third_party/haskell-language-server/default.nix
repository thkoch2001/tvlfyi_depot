{ pkgs, ... }:

rec {
  hls-nix = { ghc }: import ./hls.nix { inherit pkgs ghc; };
  ghc883 = hls-nix { ghc = "ghc883"; };
}
