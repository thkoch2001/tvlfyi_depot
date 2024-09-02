{ depot, pkgs, ... }:

let
  em = depot.tools.eaglemode;
  emSrc = pkgs.srcOnly pkgs.em;
in
em.buildPlugin {
  name = "qoi";
  version = "canon";
  src = ./.;
  target = "PlQoi";
}
