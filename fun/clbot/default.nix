{ depot, ... }@args:

let
  clbot = depot.fun.clbot;
  gopkgs = depot.third_party.gopkgs;
in
depot.nix.buildGo.program {
  name = "clbot";
  srcs = [
    ./clbot.go
  ];
  deps = [
    clbot.gerrit
    gopkgs."github.com".davecgh.go-spew.spew.gopkg
    gopkgs."github.com".golang.glog.gopkg
    gopkgs."golang.org".x.crypto.ssh.gopkg
    gopkgs."gopkg.in"."irc.v3".gopkg
  ];
}
