{ depot, ... }:

depot.nix.buildGo.package {
  name = "utils";
  srcs = [
    ./utils.go
  ];
}
