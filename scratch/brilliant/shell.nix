let
  pkgs = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    ref = "nixos-20.03";
    rev = "afa9ca61924f05aacfe495a7ad0fd84709d236cc";
  }) {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      hspec
      optparse-applicative
      unordered-containers
    ]))
  ];
}
