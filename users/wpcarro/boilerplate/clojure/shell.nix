{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    leiningen
  ];
}
