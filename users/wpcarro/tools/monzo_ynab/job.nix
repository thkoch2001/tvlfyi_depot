{ depot, ... }:

let
  inherit (depot.users.wpcarro) gopkgs;
in depot.nix.buildGo.program {
  name = "job";
  srcs = [
    ./main.go
  ];
  deps = with gopkgs; [
    kv
    utils
  ];
}
