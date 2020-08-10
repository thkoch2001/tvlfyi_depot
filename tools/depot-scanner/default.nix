{ depot, pkgs, ...}:

let
  localProto = depot.nix.buildGo.proto {
    name = "code.tvl.fyi/tools/depot-scanner/proto"
    proto = ./proto/depot_scanner.proto
  }
in depot.nix.buildGo.program {
  name = "depot-scanner";
  srcs = [
    ./main.go
  ];
  deps = [
    localProto
  ];
}
