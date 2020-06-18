{ depot, ... }:

depot.nix.buildTypedGo.program {
  name = "example";
  srcs = [
    ./main.go2
  ];
}
