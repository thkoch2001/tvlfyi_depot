{ depot, ... }:

depot.nix.buildGo.package {
  name = "code.tvl.fyi/fun/clbot/gerrit/gerritevents";
  srcs = [
    ./time.go
    ./types.go
    ./events.go
  ];
}
