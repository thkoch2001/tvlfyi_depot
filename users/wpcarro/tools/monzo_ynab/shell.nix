{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    go
    goimports
    godef
  ];
}
