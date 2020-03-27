let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.go
    pkgs.goimports
    pkgs.godef
  ];
}
