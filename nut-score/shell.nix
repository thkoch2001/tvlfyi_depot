let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  name = "nut-score";
  buildInputs = with pkgs; [
    yarn
  ];
}
