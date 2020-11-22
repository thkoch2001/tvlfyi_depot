let
  depot = import <depot> {};
  pkgs = depot.third_party;

  home = path: import path { inherit depot pkgs; };
in
  {
    whitby = home ./configs/whitby.nix;
  }

