let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-live
  ];
}
