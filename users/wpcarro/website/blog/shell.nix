{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    hugo
  ];
}
