let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs
    python3
    go
    goimports
  ];
}
