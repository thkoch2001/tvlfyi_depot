{ depot, pkgs, ... }:

let
  localProto = depot.nix.buildGo.grpc {
    name = "code.tvl.fyi/tools/depot-scanner/proto";
    proto = ./depot_scanner.proto;
  };
in
depot.nix.buildGo.program
  {
    name = "depot-scanner";
    srcs = [
      ./main.go
    ];
    deps = [
      localProto
    ];
  } // { inherit localProto; meta.ci = false; }
