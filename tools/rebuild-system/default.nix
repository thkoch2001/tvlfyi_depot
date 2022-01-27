{ depot, pkgs, lib, ... }:

let
  rebuildSystem = depot.nix.buildGo.program {
    name = "rebuild-system";
    srcs = [
      ./main.go
    ];
  };
in pkgs.writeShellScript "rebuild-system" ''
  PATH=${lib.strings.makeBinPath (with pkgs; [ nix git ])}
  ${rebuildSystem}/bin/rebuild-system "$@"
''
