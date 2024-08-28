# Derivation for my fully configured Eagle Mode.
{ depot, ... }:

let
  config = depot.tools.eaglemode.etcDir {
    extraPaths = [ depot.tools.eaglemode.commands.emacsclient ];
  };
in
depot.tools.eaglemode.withConfig {
  inherit config;
}
