# Derivation for my fully configured Eagle Mode.
{ depot, ... }:

with depot.tools.eaglemode;

withConfig {
  config = etcDir {
    extraPaths = [
      commands.emacsclient
      plugins.example
      plugins.yatracker
      plugins.qoi
      plugins.avif
    ];
  };
}
