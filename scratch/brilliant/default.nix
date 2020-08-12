let
  pkgs = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    ref = "nixos-20.03";
    rev = "afa9ca61924f05aacfe495a7ad0fd84709d236cc";
  }) {};

  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    optparse-applicative
    unordered-containers
    split
  ]);
in pkgs.stdenv.mkDerivation {
  name = "transform-keyboard";
  buildInputs = [];
  src = builtins.path {
    path = ./.;
    name = "transform-keyboard-src";
  };
  buildPhase = ''
    ${ghc}/bin/ghc ./Main.hs
  '';
  installPhase = ''
    mkdir -p $out && mv Main $out/transform-keyboard
  '';
}
