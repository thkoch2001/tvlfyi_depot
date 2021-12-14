{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs.elmPackages; [
    elm
    elm-format
    elm-live
  ];
}
