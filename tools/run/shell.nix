{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    go
  ];
}
