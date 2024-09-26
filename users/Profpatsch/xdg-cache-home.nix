{ depot, pkgs, lib, ... }:
depot.nix.writeExecline "xdg-cache-home" { } [
  "if"
  "-n"
  [
    "printenv"
    "XDG_CACHE_HOME"
  ]
  "importas"
  "HOME"
  "HOME"
  "echo"
  "\${HOME}/.cache"
]
