let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    ]))
  ];
}
