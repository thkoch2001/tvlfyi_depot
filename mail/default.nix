{ depot, ... }:

let
  inherit (builtins) fetchGit;
  tazjdots = import (fetchGit "sso://user/tazjin/dotfiles") { pkgs = depot; };
in tazjdots.services.lieer
