{ pkgs, depot, briefcase, ... }:

depot.buildGo.program {
  name = "run";
  srcs = [
    ./main.go
  ];
  deps = with briefcase.gopkgs; [
    utils
  ];
}
