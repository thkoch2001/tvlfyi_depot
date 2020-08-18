let
  briefcase = import <briefcase> {};
  pkgs = briefcase.third_party.pkgs;
in pkgs.mkShell {
  buildInputs = with pkgs.elmPackages; [
    elm
    elm-format
    elm-live
  ];
}
