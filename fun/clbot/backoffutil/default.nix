{ depot, ... }:

let
  inherit (depot.third_party) gopkgs;
in
depot.nix.buildGo.package {
  name = "code.tvl.fyi/fun/clbot/backoffutil";
  srcs = [
    ./backoffutil.go
  ];
  deps = [
    gopkgs."github.com".cenkalti.backoff.gopkg
  ];
}
