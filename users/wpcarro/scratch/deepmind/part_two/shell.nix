{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs
    python3
    go
    goimports
  ];
}
