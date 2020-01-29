with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "f-hs";
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (pkgs: [
    ]))
  ];
}
