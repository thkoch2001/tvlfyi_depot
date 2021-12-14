{ pkgs, depot, ... }:

depot.nix.buildGo.program {
  name = "run";
  srcs = [
    ./main.go
  ];
  deps = with depot.users.wpcarro.gopkgs; [
    utils
  ];
}
