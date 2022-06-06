{ depot, ... }:

let
  inherit (depot.users.sterni.external.likely-music)
    nixosModule
    likely-music
    ;
in

{
  imports = [
    ./nginx.nix
    nixosModule
  ];

  config = {
    services.likely-music = {
      enable = true;
      virtualHost = "likely-music.sterni.lv";
      package = likely-music;
    };
  };
}
