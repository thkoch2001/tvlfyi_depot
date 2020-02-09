{
  pkgs ? import <nixpkgs> {},
  briefcase ? import <briefcase> {},
  ...
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.go
    pkgs.goimports
    pkgs.godef
    briefcase.monzo_ynab.job
    briefcase.monzo_ynab.tokens
  ];
}
