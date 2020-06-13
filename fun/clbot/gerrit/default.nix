{ depot, ... }:
let
  clbot = depot.fun.clbot;
  deps = clbot.deps;
in
depot.nix.buildGo.package {
  name = "code.tvl.fyi/fun/clbot/gerrit";
  srcs = [
    ./watcher.go
  ];
  deps = [
    clbot.gerrit.gerritevents
    deps.backoff.gopkg
    deps.glog.gopkg
    deps.x-crypto.ssh.gopkg
  ];
}
