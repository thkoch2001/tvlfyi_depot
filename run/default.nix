{
  pkgs ? import <nixpkgs> {},
  depot ? import <depot> {},
  briefcase ? import <briefcase> {},
  ...
}:

depot.buildGo.program {
  name = "run";
  srcs = [
    ./main.go
  ];
  deps = with briefcase.gopkgs; [
    utils
  ];
}
