{ depot, ... }:

depot.nix.buildGo.package {
  name = "kv";
  srcs = [
    ./kv.go
  ];
}
