{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = [
    pkgs.go
    pkgs.goimports
    pkgs.godef
  ];
}
