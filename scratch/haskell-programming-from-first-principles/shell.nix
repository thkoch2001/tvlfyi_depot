let
  pkgs = import <unstable> {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      hpkgs.quickcheck-simple
      hpkgs.checkers
    ]))
  ];
}
