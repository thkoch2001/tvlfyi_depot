{ depot, ... }@args:

let
  deps = import ./deps.nix args;
  clbot = depot.fun.clbot;
in
depot.nix.buildGo.program {
  name = "clbot";
  srcs = [
    ./clbot.go
  ];
  deps = [
    clbot.gerrit
    deps.spew.spew.gopkg
    deps.glog.gopkg
    deps.x-crypto.ssh.gopkg
  ];
} // { inherit deps; }
