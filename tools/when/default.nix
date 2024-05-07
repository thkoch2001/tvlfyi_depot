{ depot, ... }:

depot.nix.buildGo.program {
  name = "when";
  srcs = [ ./when.go ];
}
