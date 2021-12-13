let
  briefcase = import <briefcase> {};
  pkgs = briefcase.third_party.pkgs;
in pkgs.mkShell {
  buildInputs = with pkgs; [
    hugo
  ];
}
