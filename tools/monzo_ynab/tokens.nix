{ depot, briefcase, ... }:

let
  auth = depot.buildGo.package {
    name = "auth";
    srcs = [
      ./auth.go
    ];
    deps = with briefcase.gopkgs; [
      utils
    ];
  };
in depot.buildGo.program {
  name = "token-server";
  srcs = [
    ./tokens.go
  ];
  deps = with briefcase.gopkgs; [
    kv
    utils
    auth
  ];
}
