{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs
    yarn
  ];
}
