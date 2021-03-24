{ pkgs, ... }:

# TODO(sterni): replace this with upstream haskell-language-server or
#               switch to easy-hls-nix as glittershark planned

rec {
  hls-nix = { ghc }: import ./hls.nix { inherit pkgs ghc; };
  ghc884 = hls-nix { ghc = "ghc884"; };
}
