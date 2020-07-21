{ depot, __findFile, ... }@args:

let
  clbot = depot.fun.clbot;
  gopkgs = depot.third_party.gopkgs;
  __findFile = x: y: (args.__findFile x y).gopkg;
in
depot.nix.buildGo.program {
  name = "clbot";
  srcs = [
    ./clbot.go
  ];
  deps = [
    clbot.gerrit
    <third_party/gopkgs/github.com/davecgh/go-spew/spew>
    <third_party/gopkgs/github.com/golang/glog>
    <third_party/gopkgs/golang.org/x/crypto/ssh>
    <third_party/gopkgs/gopkg.in/irc.v3>
  ];
}
