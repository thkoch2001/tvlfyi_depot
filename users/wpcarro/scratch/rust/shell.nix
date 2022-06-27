{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = [
    pkgs.cargo
  ];
}
