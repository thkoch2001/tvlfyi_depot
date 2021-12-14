{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      hspec
      optparse-applicative
      unordered-containers
      split
    ]))
  ];
}
