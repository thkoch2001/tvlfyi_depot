# Derivation for my fully configured Eagle Mode.
{ depot, ... }:

let
  config = depot.tools.eaglemode.etcDir {
    extraPaths = [ depot.tools.eaglemode.commands.B ];
  };
in
depot.tools.eaglemode.withConfig {
  inherit config;
}
