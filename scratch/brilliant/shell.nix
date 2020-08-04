let
  pkgs = import /home/wpcarro/nixpkgs {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      optparse-applicative
    ]))
  ];
}
