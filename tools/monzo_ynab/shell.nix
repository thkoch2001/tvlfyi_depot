let
  briefcase = import <briefcase> {};
  pkgs = briefcase.third_party.pkgs;
in pkgs.mkShell {
  buildInputs = [
    pkgs.go
    pkgs.goimports
    pkgs.godef
  ];
}
