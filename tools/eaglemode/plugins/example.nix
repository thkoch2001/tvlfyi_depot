{ depot, pkgs, ... }:

let
  em = depot.tools.eaglemode;
  emSrc = with pkgs; srcOnly eaglemode;
in
em.buildPlugin {
  name = "example";
  version = "canon";

  src = pkgs.runCommand "em-plugin-example-src" { } ''
    set -ux
    cp -r ${emSrc}/doc/examples/CppApiExamples/PluginExample $out
  '';

  target = "PlEx";
}
