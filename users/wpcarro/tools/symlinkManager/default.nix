{ depot, ... }:

let
  inherit (depot.users.wpcarro) gopkgs;
in
depot.nix.buildGo.program {
  name = "symlink-mgr";
  srcs = [
    ./main.go
  ];
  deps = with gopkgs; [
    utils
  ];
}
