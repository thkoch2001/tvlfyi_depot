{ depot ? import ../../../../.. {}
, pkgs ? depot.third_party.nixpkgs
, ...
}:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.cbqn
  ];
}
