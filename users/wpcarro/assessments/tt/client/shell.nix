{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-live
  ];
}
