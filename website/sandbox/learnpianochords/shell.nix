let
  pkgs = import /home/wpcarro/nixpkgs {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-live
  ];
}
