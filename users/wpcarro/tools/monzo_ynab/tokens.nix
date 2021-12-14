{ depot, ... }:

let
  inherit (depot.users.wpcarro) gopkgs;

  auth = depot.nix.buildGo.package {
    name = "auth";
    srcs = [
      ./auth.go
    ];
    deps = with gopkgs; [
      utils
    ];
  };
in depot.nix.buildGo.program {
  name = "token-server";
  srcs = [
    ./tokens.go
  ];
  deps = with gopkgs; [
    kv
    utils
    auth
  ];
}
